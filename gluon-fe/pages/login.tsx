import { getCsrfToken, signIn, useSession } from "next-auth/react"
import { Container, Row, Col, Card, Text, Input, Button, Spacer, Loading, Link } from '@nextui-org/react';
import { useCallback, useState } from "react";

export default function Login() {
  const { data: session, status } = useSession()
  const [loading, setLoading] = useState(false);
  const [email, setEmail] = useState("");
  const doSignIn = useCallback(() => {
    setLoading(true);
    signIn("email", { email });
    return false;
  }, [setLoading, email]);

  return (
    <Container xs css={{ pt: 80 }}>
      <Col>
        <Row><Text size={16} color="primary">Gluon</Text></Row>
        <Row><Text h1 size={36}>Sign in</Text></Row>
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

        <form style={{ display: "flex", flexDirection: "column" }}>
          <Row css={{ pb: 20 }}>
            <Input css={{ flexGrow: 1 }} placeholder="Email" name="email" value={email} onChange={e => setEmail(e.target.value)} />
          </Row>
          <Row css={{ pb: 20 }}>
            <Button css={{ flexGrow: 1 }} type="submit" disabled={loading} onClick={doSignIn}>
              {loading ? <Loading color="currentColor" size="sm" /> : "Continue"}</Button>
          </Row>
        </form>

        <Text color="gray">
          This Gluon instance is not public.
        </Text>

        <Text color="gray" css={{ pt: 20 }}>
          Meanwhile, you can <Link style={{ display: "inline" }} href="https://github.com/losfair/gluon" target="_blank">host your own Gluon instance</Link>.
        </Text>
      </Col>
    </Container>
  )
}

/*



  if (status === "authenticated") {
    return <p>Signed in as {session.user!.email}</p>
  }

  return <a href="/api/auth/signin">Sign in</a>
*/