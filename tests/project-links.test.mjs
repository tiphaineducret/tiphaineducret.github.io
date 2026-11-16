import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = fileURLToPath(new URL('..', import.meta.url));
const html = readFileSync(join(root, 'index.html'), 'utf8');
const css = readFileSync(join(root, 'styles.css'), 'utf8');

assert.equal(
  (html.match(/class="project-detail-link"/g) ?? []).length,
  13,
  'les treize projets doivent conserver un lien vers leur page',
);
assert.equal(
  (html.match(/>Voir le projet <span class="project-detail-link__icon" aria-hidden="true"><\/span><\/a>/g) ?? []).length,
  13,
  'les treize liens doivent partager le même libellé et la même icône',
);
assert.match(css, /\.project-detail-link\s*\{[^}]*display:\s*inline-grid/s, 'les liens doivent être présentés comme des boutons éditoriaux');
assert.match(css, /\.project-detail-link:hover\s*\{[^}]*background:\s*var\(--blue\)/s, 'le survol doit être clairement visible');
assert.match(css, /\.project-detail-link:focus-visible/s, 'le focus clavier doit être visible');

console.log('Liens projets validés.');
