import { Container, Col, Row, Card, Spacer, Button, Input, Loading, Text, Image, Link } from '@nextui-org/react'
import type { NextPage } from 'next'
import { useSession } from 'next-auth/react'
import dynamic from 'next/dynamic'
import { useRecoilValueLoadable } from 'recoil'
import { RequireAuth } from '../../../../components/require_auth'
import { RequireProject } from '../../../../components/require_project'
import { appListQuery, appSelector, projectSelector } from '../../../../feutil/state'
import { loadProjectProps, ProjectProps } from '../../../../service/util'
import NextLink from 'next/link'
import type { App } from '../../../../models'
import { Footer } from '../../../../components/footer'
import { useRouter } from 'next/router'
import { AppCard } from '../../../../components/app_card'

export const getServerSideProps = loadProjectProps;

const SingleApp: NextPage<ProjectProps> = ({ projectId, userId }) => {
  const router = useRouter();
  const appId = parseInt(router.query.appId as string);
  const app = useRecoilValueLoadable(appSelector([projectId, appId]))

  return (
    <RequireAuth>
      <RequireProject>
        <Container xs css={{ pt: 80 }}>
          <Col>
            <Row align="center">
              <Col css={{ width: "auto" }}>
                <Text h1 size={36}>Manage App</Text>
              </Col>
            </Row>
            {app.state === "hasValue" && !!app.contents && <>
              <AppCard app={app.contents} inSingleAppPage />
            </>}
          </Col>
          <Footer projectId={projectId} userId={userId} />
        </Container>
      </RequireProject>
    </RequireAuth>
  )
}

export default dynamic(() => Promise.resolve(SingleApp), {
  ssr: false
});
