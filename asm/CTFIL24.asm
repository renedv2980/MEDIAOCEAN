*          DATA SET CTFIL24    AT LEVEL 039 AS OF 08/10/11                      
*&&      SET   NOP=N                                                            
*PHASE TA1324C                                                                  
FIL24    TITLE 'BDE EMAIL RECORD DISPLAY'                                       
FIL24    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CTFIL24*,R7,RR=RE                                              
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         ST    R1,CALLR1                                                        
         MVC   SVPARMS,0(R1)                                                    
*                                                                               
         SRL   RF,24                                                            
         CLM   RF,1,=AL1(ROUTSN)                                                
         BNH   *+6                 UNKNOWN ROUTINE                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
         SPACE 1                                                                
ROUTS    DS    0XL4                                                             
         B     OBJECT              OBJECT INVOKER                               
         B     INIT                INITIALIZATION CALL                          
*                                                                               
ROUTSN   EQU   (*-ROUTS)/4                                                      
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         SPACE 1                                                                
EXIT     L     R1,CALLR1                                                        
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       FILTER RETURN LOW                            
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       FILTER RETURN EQUAL                          
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       FILTER RETURN HIGH                           
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       FILTER RETURN NOT WANTED                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* TABLE  ITERATING ROUTINE - R1=EQUATED VERB                          *         
*                          - RF=A(TABLE)                              *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** SET HIGH IF NOT WANT OVERRIDE                
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE                                      
*                                                                               
ITER02   ICM   RF,15,OBJADR        INVOKE OBJECT                                
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                  *         
*                                                                     *         
* P1 HOLDS EQUATED VERB                                               *         
***********************************************************************         
         SPACE 1                                                                
OBJECT   L     R1,SVPARMS                                                       
         LA    RF,TABLEOO                                                       
         B     ITER                                                             
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* KEY OBJECT                                                          *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
KEY      LM    R0,R3,SVPARMS                                                    
         USING GBDED,R2                                                         
         LA    RF,KEYTABL                                                       
         B     ITER                                                             
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(KLAST),AL1(0,0,0),AL4(KEYLAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
KEYFRST  L     R1,SVPARMS+12       EQUATED VERB                                 
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
KFKVAL   XC    GBDEKEY,GBDEKEY                                                  
         MVI   GBDEID,GBDEIDQ                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    GBDEKEY,GBDEKEY                                                  
         MVI   GBDEID,GBDEIDQ                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR KEY OBJECT                                            *         
***********************************************************************         
         SPACE 1                                                                
KEYLAST  L     R1,SVPARMS+12       EQUATED VERB                                 
         LA    RF,KLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
KLTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KLKVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A KEY OBJECT                              *         
***********************************************************************         
         SPACE 1                                                                
KLKVAL   L     RF,ATLST                                                         
         USING TLSTD,RF                                                         
         MVC   GBDEKEY,TLRKEY                                                   
         B     EXITOK                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT IDENTIFIER                                  *         
* P2 HOLDS EQUATED DATA IDENTIFIER OR 0 IF GLOBAL DATA ACTION         *         
* P3 BYTE  0   HOLDS EQUATED DATA VERB IF P2 IS ZERO                  *         
* P3 BYTES 1-3 HOLDS EQUATED ACTION VERB                              *         
* P4 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P5 HOLDS A(FIELD TABLE ENTRY) OR ZERO IF P2 IS ZERO                 *         
*                                                                     *         
* FIELD DATA IS EXTRACTED/OUTPUT INTO FVIFLD                          *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
DATA     ICM   R1,15,SVPARMS+4     RE HOLDS DATA IDENTIFIER                     
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS+12                                                    
         USING GBDED,R2                                                         
         XR    R1,R1                                                            
         IC    R1,SVPARMS+8        GET GLOBAL VERB                              
         LA    RF,DTATABL                                                       
         B     ITER                                                             
*                                                                               
DATA02   LA    RF,KNOWTAB          TABLE OF KNOWN OBJECTS                       
         USING KNOWTABD,RF                                                      
*                                                                               
DATA04   CLC   KNOWID,=AL2(EOT)    REACH END - NOT A KNOWN DATA TYPE            
         BE    EXITH                                                            
         CLM   R1,3,KNOWID         IS THIS A KNOWN TYPE?                        
         BE    DATA06                                                           
         LA    RF,KNOWLQ(RF)                                                    
         B     DATA04                                                           
         SPACE 1                                                                
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
         LM    R1,R3,SVPARMS+8     R1 HOLDS VERB                                
         USING GBDED,R2            R2 HOLDS A(RECORD)                           
         BR    RF                                                               
*                                                                               
DTATABL  DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(01011),AL4(LSTNDTA)       LAST NAME                          
         DC    AL2(01012),AL4(FSTNDTA)       FIRST NAME                         
         DC    AL2(01013),AL4(EMLDTA)        EMAIL                              
         DC    AL2(01014),AL4(ORGNDTA)       ORGANISATION                       
         DC    AL2(01015),AL4(STKDTA)        START KEY                          
         DC    AL2(01016),AL4(DSKDTA)        DISK ADDRESS                       
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL24    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ORGANISATION                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING GBDED,R2            R2 HOLDS A(RECORD)                           
ORGNDTA  LA    RF,ORGNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
ORGNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISORGN)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTORGN)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTORGN)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTORGN)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ORGANISATION                                                *         
***********************************************************************         
         SPACE 1                                                                
DISORGN  MVC   FVIFLD(L'GBDEORG),GBDEORG                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY ORGANISATION FILTER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DFLTORGN MVC   FVIFLD(L'GBDEORG),GBDEORG                                        
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE ORGANISATION FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
VFLTORGN MVC   GBDEORG,FVIFLD                                                   
         MVC   FLTIFLD(L'GBDEORG),FVIFLD                                        
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DO FILTERING FOR SYSTEM                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTORGN CLC   GBDEORG,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR START KEY                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING GBDED,R2            R2 HOLDS A(RECORD)                           
STKDTA   LA    RF,STKNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
STKNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTKN)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTSTKN)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTSTKN)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTSTKN)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY START KEY                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISSTKN  MVC   FVIFLD(L'GBDENAME),GBDENAME                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY START KEY AS FILTER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DFLTSTKN MVC   FVIFLD(L'GBDENAME),GBDENAME                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE START KEY FILTER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VFLTSTKN MVC   GBDENAME,FVIFLD                                                  
         MVC   FLTIFLD(L'GBDENAME),FVIFLD                                       
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DO FILTERING FOR START KEY                                          *         
***********************************************************************         
         SPACE 1                                                                
DOFTSTKN CLC   GBDENAME,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LAST NAME                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
LSTNDTA  LA    RF,LSTNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
LSTNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLSTN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LAST NAME                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISLSTN  MVC   FVIFLD(L'TLLSTN),TLLSTN                                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FIRST NAME                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FSTNDTA  LA    RF,FSTNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
FSTNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISFSTN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FIRST NAME                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISFSTN  MVC   FVIFLD(L'TLFSTN),TLFSTN                                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR EMAIL ID                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
EMLDTA   LA    RF,EMLTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
EMLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISEML)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY EMAIL ID                                                    *         
***********************************************************************         
         SPACE 1                                                                
DISEML   MVC   FVIFLD(L'TLEMAIL),TLEMAIL                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISK ADDRESS                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
DSKDTA   LA    RF,DSKTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
DSKTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDSK)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DISK ADDRESS                                                *         
***********************************************************************         
         SPACE 1                                                                
DISDSK   GOTO1 VHEXOUT,BOPARM,TLRDA,FVIFLD,L'TLRDA                              
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
***********************************************************************         
         DROP  R2                                                               
         SPACE 1                                                                
LIST     LM    R0,R2,SVPARMS                                                    
THIS     USING GBDED,R2                                                         
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LINIT),AL1(0,0,0),AL4(INITL)                                 
         DC    AL1(LTSARDIR),AL1(0,0,0),AL4(TSARDIR)                            
         DC    AL1(LTSARFIL),AL1(0,0,0),AL4(TSARFIL)                            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALISE LIST                                                     *         
***********************************************************************         
         SPACE 1                                                                
INITL    MVC   LSNUMHED,=AL2(1)                                                 
*        MVC   LSCOLROW,=AL2(160)     <== 2 LINES                               
*        MVC   LSCOLLIN,=AL2(160)                                               
*        OI    LSSTAT2,LSSXSPAC                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LIST PAGE                                                 *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(GBDLEN),THIS.GBDED                                         
         ICM   R1,15,=AL4(XIO11+XOGENDIR+XOHIGH)                                
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL                                                            
         B     NLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     ICM   R1,15,=AL4(XIO11+XOSEQ+XOGENDIR)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST02   CLI   IOKEY+(GBDEID-GBDED),GBDEIDQ                                     
         BNE   EXITL                                                            
         MVC   THIS.GBDED(GBDLEN),IOKEY     WE WANT THIS KEY HERE...            
         B     EXITOK                                                           
         DROP  THIS                                                             
         EJECT                                                                  
***********************************************************************         
* SET UP DIRECTORY                                                    *         
***********************************************************************         
         SPACE 1                                                                
TSARDIR  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET UP FILE                                                         *         
***********************************************************************         
         SPACE 1                                                                
TSARFIL  LM    R2,R3,SVPARMS3                                                   
         USING TLSTD,R3                                                         
         MVC   TLRLEN,=AL2(TLLNQ)  SET NEW RECORD LENGTH                        
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('GBNIDQ',(R2)),0                  
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         USING GBNELD,RF                                                        
         MVC   TLORG,GBNORG                                                     
         MVC   TLLSTN,GBNFNM                                                    
         MVC   TLFSTN,GBNFNM+20                                                 
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('GBEIDQ',(R2)),0                  
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         USING GBEELD,RF                                                        
         MVC   TLEMAIL,GBEEMAIL                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
FFS      DC    16X'FF'                                                          
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
OVERWRKD DSECT                                                                  
CALLR1   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
SVKEY    DS    CL32                                                             
         EJECT                                                                  
***********************************************************************         
* OTHER DSECTS                                                        *         
***********************************************************************         
         SPACE 1                                                                
*        CTFILWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE CTFILWORK                                                      
         PRINT ON                                                               
*        GEGENASS                                                               
         PRINT OFF                                                              
       ++INCLUDE GEGENBDE                                                       
         PRINT ON                                                               
*        CTMSGEQUS                                                              
         PRINT OFF                                                              
       ++INCLUDE CTMSGEQUS                                                      
         PRINT ON                                                               
*        DDFLDHDR                                                               
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*                                                                               
TLSTD    DSECT                                                                  
         ORG   TLUSER                                                           
TLORG    DS    CL7                                                              
TLFSTN   DS    CL20                                                             
TLLSTN   DS    CL20                                                             
TLEMAIL  DS    CL64                                                             
TLLNQ    EQU   *-TLSTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039CTFIL24   08/10/11'                                      
         END                                                                    
