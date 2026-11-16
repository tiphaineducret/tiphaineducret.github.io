const revealItems = document.querySelectorAll('.reveal');

if ('IntersectionObserver' in window) {
  const revealObserver = new IntersectionObserver((entries, observer) => {
    for (const entry of entries) {
      if (!entry.isIntersecting) continue;
      entry.target.classList.add('is-visible');
      observer.unobserve(entry.target);
    }
  }, { threshold: 0.14, rootMargin: '0px 0px -6% 0px' });

  revealItems.forEach((item) => revealObserver.observe(item));
} else {
  revealItems.forEach((item) => item.classList.add('is-visible'));
}

const navLinks = [...document.querySelectorAll('.main-nav a[href^="#"]')];
const observedSections = navLinks
  .map((link) => document.querySelector(link.getAttribute('href')))
  .filter(Boolean);

const setActiveNavLink = (id) => {
  navLinks.forEach((link) => {
    link.classList.toggle('is-active', link.getAttribute('href') === `#${id}`);
  });
};

if ('IntersectionObserver' in window) {
  const navObserver = new IntersectionObserver((entries) => {
    const visible = entries.find((entry) => entry.isIntersecting);
    if (!visible) return;
    setActiveNavLink(visible.target.id);
  }, { rootMargin: '-35% 0px -55% 0px' });

  observedSections.forEach((section) => navObserver.observe(section));
} else {
  const updateActiveNavLinkOnScroll = () => {
    const current = observedSections.find((section) => {
      const rect = section.getBoundingClientRect();
      return rect.top <= window.innerHeight * 0.45 && rect.bottom >= window.innerHeight * 0.45;
    });
    if (current) setActiveNavLink(current.id);
  };
  window.addEventListener('scroll', updateActiveNavLinkOnScroll, { passive: true });
  updateActiveNavLinkOnScroll();
}

const lightbox = document.querySelector('.lightbox');
const lightboxImage = lightbox?.querySelector('img');

if (lightbox && lightboxImage) {
  document.querySelectorAll('[data-lightbox]').forEach((trigger) => {
    trigger.addEventListener('click', () => {
      const preview = trigger.closest('.project-chapter')?.querySelector('.project-visual img');
      lightboxImage.src = trigger.dataset.lightbox;
      lightboxImage.alt = preview?.alt ?? 'Aperçu du projet';
      lightbox.showModal();
    });
  });

  lightbox.querySelector('.lightbox-close')?.addEventListener('click', () => lightbox.close());
  lightbox.addEventListener('click', (event) => {
    if (event.target === lightbox) lightbox.close();
  });
}

const tabs = [...document.querySelectorAll('[role="tab"]')];
tabs.forEach((tab) => {
  tab.addEventListener('click', () => {
    tabs.forEach((candidate) => {
      const selected = candidate === tab;
      candidate.setAttribute('aria-selected', String(selected));
      document.getElementById(candidate.getAttribute('aria-controls')).hidden = !selected;
    });
  });

  tab.addEventListener('keydown', (event) => {
    if (!['ArrowLeft', 'ArrowRight'].includes(event.key)) return;
    event.preventDefault();
    const direction = event.key === 'ArrowRight' ? 1 : -1;
    tabs[(tabs.indexOf(tab) + direction + tabs.length) % tabs.length].focus();
  });
});
