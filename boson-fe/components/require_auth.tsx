import { useSession } from "next-auth/react";
import { useEffect } from "react";
import { useRouter } from "next/router";

export function RequireAuth({ children }: { children: React.ReactNode }) {
  const { status } = useSession();
  const router = useRouter();

  useEffect(() => {
    if (status === "unauthenticated") {
      router.push("/login");
    }
  }, [status, router]);

  return <>{children}</>;
}