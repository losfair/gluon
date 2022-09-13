import { Container, Col, Row, Card, Spacer, Button, Input, Loading, Text } from '@nextui-org/react'
import loading from '@nextui-org/react/types/loading'
import type { NextPage } from 'next'
import { useSession } from 'next-auth/react'
import dynamic from 'next/dynamic'
import { RequireAuth } from '../components/require_auth'
import { RequireProject } from '../components/require_project'

const Dashboard: NextPage = () => {
  const { data: session, status } = useSession();

  return (
    <RequireAuth>
      <RequireProject>
        <Container xs css={{ pt: 80 }}>
          <Col>
            <Row><Text h1 size={36}>Dashboard</Text></Row>
            {status === "authenticated" && (
              <Row css={{ pb: 20 }}>
                <Card variant="bordered">
                  <Card.Body css={{ py: 10 }}>
                    <Row justify="flex-start" align="center">
                      <Text size={15}>
                        Currently signed in as {session.user!.email}
                      </Text>
                      <Spacer css={{ flexGrow: "1" }} />
                      <Button light color="error" auto>Sign out</Button>
                    </Row>
                  </Card.Body>
                </Card>
              </Row>
            )}
          </Col>
        </Container>
      </RequireProject>
    </RequireAuth>
  )
}

export default dynamic(() => Promise.resolve(Dashboard), {
  ssr: false
});
