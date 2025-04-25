import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

// This runs in Node.js - Don't use client-side code here (browser APIs, JSX...)

const config: Config = {
  title: "Langchain hs",
  tagline: 'Build LLM powered applications with Haskell',
  favicon: 'img/favicon.ico',

  // Set the production url of your site here
  url: 'https://tusharad.github.io',
  // Set the /<baseUrl>/ pathname under which your site is served
  // For GitHub pages deployment, it is often '/<projectName>/'
  baseUrl: '/langchain-hs/',
  // GitHub pages deployment config.
  // If you aren't using GitHub pages, you don't need these.
  organizationName: 'tusharad', // Usually your GitHub org/user name.
  projectName: 'langchain-hs', // Usually your repo name.

  onBrokenLinks: 'ignore',
  onBrokenMarkdownLinks: 'warn',

  // Even if you don't use internationalization, you can use this field to set
  // useful metadata like html lang. For example, if your site is Chinese, you
  // may want to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      {
        docs: {
          sidebarPath: './sidebars.ts',
          // Please change this to your repo.
          // Remove this to remove the "edit this page" links.
          editUrl:
            'https://github.com/tusharad/langchain-hs',
        },
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    // Replace with your project's social card
    image: 'img/docusaurus-social-card.jpg',
    navbar: {
      title: 'Langchain-hs',
      logo: {
        alt: 'Langchain-hs logo',
        src: 'img/logo.svg',
      },
      items: [
        {
            href: "https://python.langchain.com/en/latest/",
            label: "Python Docs",
            position: "left",
          },
          {
            to: "/docs/",
            label: "Go Docs",
            position: "left",
          },
          // Please keep GitHub link to the right for consistency.
          {
            href: "https://github.com/tusharad/langchain-hs",
            label: "GitHub",
            position: "right",
          },      
      ],
    },
    footer: {
      style: 'dark',
      links: [
        {
          title: 'Docs',
          items: [
            {
              label: 'Tutorial',
              to: '/docs/intro',
            },
          ],
        },
        {
          title: 'Community',
          items: [
            
          ],
        },
        {
          title: 'More',
          items: [
            {
              label: 'GitHub',
              href: 'https://github.com/tusharad/langchain-hs',
            },
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} Tushar Adhatrao, Inc. Built with Docusaurus.`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
      additionalLanguages: ['haskell']
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
