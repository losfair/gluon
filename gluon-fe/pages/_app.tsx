import '../styles/globals.css'
import type { AppProps } from 'next/app'
import { NextUIProvider } from '@nextui-org/react';
import { SessionProvider } from 'next-auth/react';
import { RecoilRoot } from 'recoil';

function MyApp({ Component, pageProps }: AppProps) {
  return (
    <RecoilRoot>
      <NextUIProvider>
        <SessionProvider>
          <Component {...pageProps} />
        </SessionProvider>
      </NextUIProvider>
    </RecoilRoot>
  );
}

export default MyApp
