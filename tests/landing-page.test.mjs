import assert from 'node:assert/strict';
import { existsSync, readFileSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = fileURLToPath(new URL('..', import.meta.url));
const html = readFileSync(join(root, 'index.html'), 'utf8');
const css = readFileSync(join(root, 'styles.css'), 'utf8');

assert.match(html, /<section class="hero" id="accueil"/, 'la racine GitHub doit afficher le nouvel accueil data');
assert.match(html, /<link rel="stylesheet" href="styles\.css">/, 'le nouvel accueil doit charger sa feuille de style');
assert.match(html, /<script src="script\.js" defer><\/script>/, 'les interactions du nouvel accueil doivent être chargées');
assert.doesNotMatch(
  css,
  /\.main-nav a:nth-child\(2\),\s*\.main-nav a:nth-child\(3\)\s*\{\s*display:\s*none/,
  'la navigation mobile doit garder les quatre sections accessibles',
);

const expectedProjectPages = [
  ...Array.from({ length: 6 }, (_, index) => `projets/S1/projet${index + 1}/s1-projet${index + 1}.html`),
  ...Array.from({ length: 7 }, (_, index) => `projets/S2/projet${index + 1}/s2-projet${index + 1}.html`),
];

for (const projectPath of expectedProjectPages) {
  assert.match(html, new RegExp(`href="${projectPath.replaceAll('.', '\\.')}`), `l’index doit relier ${projectPath}`);
  assert.ok(existsSync(join(root, projectPath)), `${projectPath} doit exister dans le dépôt publié`);
}

for (const assetPath of [
  'assets/cv-tiphaine-ducret.pdf',
  'assets/portrait-original-cutout.png',
  'assets/modelisation-immobiliere.jpg',
  'assets/qualyse-dashboard.jpg',
  'assets/demographie-rurale.png',
]) {
  assert.ok(existsSync(join(root, assetPath)), `${assetPath} doit être publié avec l’index`);
}

for (const projectPath of expectedProjectPages) {
  const detail = readFileSync(join(root, projectPath), 'utf8');
  assert.match(detail, /href="\/#projets" class="back-link"/, `${projectPath} doit revenir à la section projets du nouvel index`);
}

console.log('Page d’accueil GitHub validée.');
