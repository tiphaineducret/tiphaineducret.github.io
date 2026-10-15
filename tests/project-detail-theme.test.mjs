import assert from 'node:assert/strict';
import { existsSync, readFileSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = fileURLToPath(new URL('..', import.meta.url));
const projectPages = [
  ...Array.from({ length: 6 }, (_, index) => `projets/S1/projet${index + 1}/s1-projet${index + 1}.html`),
  ...Array.from({ length: 7 }, (_, index) => `projets/S2/projet${index + 1}/s2-projet${index + 1}.html`),
];
const stylesheet = join(root, 'projets/project-detail-theme.css');

assert.equal(projectPages.length, 13, 'les treize fiches publiées doivent être couvertes');
assert.ok(existsSync(stylesheet), 'la feuille de style commune aux fiches projets doit exister');

for (const relativePath of projectPages) {
  const html = readFileSync(join(root, relativePath), 'utf8');
  assert.match(
    html,
    /<link rel="stylesheet" href="\.\.\/\.\.\/project-detail-theme\.css">/,
    `${relativePath} doit charger le nouveau thème éditorial`,
  );
}

const css = readFileSync(stylesheet, 'utf8');
assert.match(css, /--paper:\s*#f7f4ed/, 'le fond papier du nouvel index doit être repris');
assert.match(css, /--blue:\s*#075cf6/, 'le bleu principal du nouvel index doit être repris');
assert.match(css, /\.section\s*>\s*:not\(h2\)/, 'les sections doivent utiliser la composition éditoriale à deux colonnes');
assert.match(css, /@media\s*\(max-width:\s*760px\)/, 'les fiches doivent rester lisibles sur mobile');
assert.match(css, /:focus-visible/, 'la navigation clavier doit rester visible');

console.log('Thème des fiches projets validé.');
