*          DATA SET CTFIL28    AT LEVEL 008 AS OF 04/19/18                      
*PHASE TA1328A                                                                  
FIL28    TITLE 'RADIO RECORD DISPLAY'                                           
FIL28    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CTFIL28*,R7,RR=RE                                              
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
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         SPACE 1                                                                
EXIT     L     R1,CALLR1                                                        
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL                                                            
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
         DC    AL1(OSES),AL1(0,0,0),AL4(SES)                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SESSION OBJECT                                                      *         
***********************************************************************         
         SPACE 1                                                                
SES      LM    R0,R3,SVPARMS                                                    
         USING SSAVD,R2                                                         
         LA    RF,SESTABL                                                       
         B     ITER                                                             
*                                                                               
SESTABL  DC    AL1(SNTROUT),AL1(0,0,0),AL4(NTROUT)                              
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* SET PARMS FOR NTRSES                                                *         
***********************************************************************         
         SPACE 1                                                                
NTROUT   OI    SNINDS1,SNIPARMS                                                 
         B     EXITOK                                                           
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
         USING CT9ARECD,R2                                                      
         LA    RF,KEYTABL                                                       
         B     ITER                                                             
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
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
KFKVAL   XC    CT9AKEY,CT9AKEY                                                  
         MVI   CT9AKTYP,CT9AKTYQ                                                
         MVI   CT9AKMED,CT9AKMRD                                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT FILTER                      *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    CT9AKEY,CT9AKEY                                                  
         MVI   CT9AKTYP,CT9AKTYQ                                                
         MVI   CT9AKMED,CT9AKMRD                                                
         B     EXITOK                                                           
         SPACE 2                                                                
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
         USING CT9ARECD,R2                                                      
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
*                                                                               
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
         LM    R1,R3,SVPARMS+8     R1 HOLDS VERB                                
         USING CT9ARECD,R2         R2 HOLDS A(RECORD)                           
         BR    RF                                                               
*                                                                               
DTATABL  DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(0201),AL4(UIDDTA)        UNIQUE CODE                         
         DC    AL2(0202),AL4(CALLDTA)       CALL LETTERS                        
         DC    AL2(0203),AL4(MEDDTA)        MEDIA                               
         DC    AL2(0213),AL4(CH1DTA)        CALL HISTORY                        
         DC    AL2(0243),AL4(SRCDTA)        OWNER SOURCE                        
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL28    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR OWNER SOURCE                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING CT9ARECD,R2         R2 HOLDS A(RECORD)                           
SRCDTA   LA    RF,SRCETBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
SRCETBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISSRCE)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSRCE)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTSRCE)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTSRCE)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTSRCE)                               
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY OWNER SOURCE                                                *         
***********************************************************************         
         SPACE 1                                                                
DISSRCE  DS    0H                   PROTECTED                                   
         MVC   FVIFLD(L'CT99KSRC),CT9AKSRC                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE OWNER SOURCE                                               *         
***********************************************************************         
         SPACE 1                                                                
VALSRCE  CLI   FVIFLD,CT99KOMS      C'S' - MSTREET                              
         BE    VSRCE10                                                          
         CLI   FVIFLD,CT99KOSD      C'R' - SDRS                                 
         BE    VSRCE10                                                          
         CLI   FVIFLD,CT99KOMF      C'F' - MEDIAFRAMEWORKS                      
         BE    VSRCE10                                                          
         CLI   FVIFLD,CT99KODS      C'D' - DS SPOT                              
         BNE   EXITNV                                                           
VSRCE10  MVC   CT9AKSRC,FVIFLD                                                  
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY OWNER SOURCE FILTER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DFLTSRCE MVC   FVIFLD(L'CT9AKSRC),FLTIFLD                                       
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE OWNER SOURCE FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
VFLTSRCE CLI   FVIFLD,CT99KOMS      C'S' - MSTREET                              
         BE    VFSRCE10                                                         
         CLI   FVIFLD,CT99KOSD      C'R' - SDRS                                 
         BE    VFSRCE10                                                         
         CLI   FVIFLD,CT99KOMF      C'F' - MEDIAFRAMEWORKS                      
         BE    VFSRCE10                                                         
         CLI   FVIFLD,CT99KODS      C'D' - DS SPOT                              
         BNE   EXITNV                                                           
VFSRCE10 MVC   FLTIFLD(L'CT9AKSRC),FVIFLD                                       
         MVC   CT9AKSRC,FVIFLD                                                  
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DO FILTERING FOR UNIQUE ID                                          *         
***********************************************************************         
         SPACE 1                                                                
DOFTSRCE CLC   CT9AKSRC,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR MEDIA                                                         
***********************************************************************         
         SPACE 1                                                                
         USING CT9ARECD,R2         R2 HOLDS A(RECORD)                           
MEDDTA   LA    RF,MEDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
MEDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMED)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMED)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTMED)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTMED)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTMED)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY MEDIA                                                                 
***********************************************************************         
         SPACE 1                                                                
DISMED   DS    0H                   PROTECTED                                   
         MVC   FVIFLD(L'CT9AKMED),CT9AKMED                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE MEDIA                                                                
***********************************************************************         
         SPACE 1                                                                
VALMED   OI    FVIFLD,C' '                                                      
         CLI   FVIFLD,C'R'          Radio                                       
         BE    VMED10                                                           
         CLI   FVIFLD,C'T'          TV                                          
         BE    VMED10                                                           
         CLI   FVIFLD,C'X'          Net Radio                                   
         BNE   EXITNV                                                           
VMED10   MVC   CT9AKMED,FVIFLD                                                  
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY MEDIA FILTER FIELD                                                    
***********************************************************************         
         SPACE 1                                                                
DFLTMED  MVC   FVIFLD(L'CT9AKMED),FLTIFLD                                       
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE MEDIA FILTER FIELD                                                   
***********************************************************************         
         SPACE 1                                                                
VFLTMED  OI    FVIFLD,C' '                                                      
         CLI   FVIFLD,C'R'          Radio                                       
         BE    VFMED10                                                          
         CLI   FVIFLD,C'T'          TV                                          
         BE    VFMED10                                                          
         CLI   FVIFLD,C'X'          Net Radio                                   
         BNE   EXITNV                                                           
VFMED10  MVC   FLTIFLD(L'CT9AKSRC),FVIFLD                                       
         MVC   CT9AKMED,FVIFLD                                                  
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DO FILTERING FOR MEDIA                                                        
***********************************************************************         
         SPACE 1                                                                
DOFTMED  CLC   CT9AKMED,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR UID                                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CT9ARECD,R2         R2 HOLDS A(RECORD)                           
UIDDTA   LA    RF,UIDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
UIDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISUID)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VUID)                                   
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISUIDN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY UNIQUE ID                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISUID   MVC   FVIFLD(L'CT9AUID),CT9AUID                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY UNIQUE ID FOR NTRSES                                        *         
***********************************************************************         
         SPACE 1                                                                
DISUIDN  L     R2,AIOREC                                                        
         MVC   FVIFLD(L'CT9AUID),CT9AUID                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE UNIQUE ID                                                  *         
***********************************************************************         
         SPACE 1                                                                
VUID     MVC   CT9AUID,FVIFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CALL LETTERS                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CT9ARECD,R2         R2 HOLDS A(RECORD)                           
CALLDTA  LA    RF,CALLTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CALLTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCALL)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISCALL)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTCALL)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTCALL)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTCALL)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY CALL LETTERS                                                *         
***********************************************************************         
         SPACE 1                                                                
DISCALL  MVC   FVIFLD(L'CT9AKCLL),CT9AKCLL                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY CALL LETTERS FILTER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DFLTCALL XR    RF,RF                                                            
         IC    RF,FLTIFLD                                                       
         BNE   EXITOK                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),FLTIFLD+1                                              
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE CALL LETTERS FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
VFLTCALL MVC   FLTIFLD(1),FVXLEN                                                
         XR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLTIFLD+1(0),FVIFLD                                              
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CT9AKCLL(0),FVIFLD                                               
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DO FILTERING FOR CALL LETTERS                                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTCALL XR    RF,RF                                                            
         IC    RF,FLTIFLD                                                       
         EX    RF,DFCLCLC                                                       
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
*                                                                               
DFCLCLC  CLC   CT9AKCLL(0),FLTIFLD+1                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CALL HISTORY 1                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CT9ARECD,R2         R2 HOLDS A(RECORD)                           
CH1DTA   LA    RF,CH1TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CH1TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCH1)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY CALL HISTORY 1                                              *         
***********************************************************************         
         SPACE 1                                                                
DISCH1   OC    CT9AKDTE,CT9AKDTE                                                
         BNZ   *+14                                                             
         MVC   FVIFLD(07),=CL07'Current'                                        
         B     EXITOK                                                           
*                                                                               
         XC    CT9AKDTE,=XL2'FFFF'                                              
         GOTO1 VDATCON,BOPARM,(2,CT9AKDTE),(13,FVIFLD)                          
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
THIS     USING CT9ARECD,R2                                                      
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LINIT),AL1(0,0,0),AL4(INITL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALISE LIST                                                     *         
***********************************************************************         
         SPACE 1                                                                
INITL    B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LIST PAGE                                                 *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'CT9AKEY),THIS.CT9ARECD                                   
         ICM   R1,15,=AL4(XOHI+XOCONFIL)                                        
         A     R1,SVPARMS5                                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL                                                            
         B     NLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     MVC   IOKEY(L'CT9AKEY),THIS.CT9ARECD                                   
         XR    RF,RF                                                            
         IC    RF,IOKEY+L'CT9AKEY-1                                             
         AHI   RF,1                                                             
         STC   RF,IOKEY+L'CT9AKEY-1                                             
*                                                                               
         ICM   R1,15,=AL4(XOHI+XOCONFIL)                                        
         A     R1,SVPARMS5                                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST02   L     RF,SVPARMS5         CONTROL FILE RECORDS ARE RETURNED IN         
         SRL   RF,4-2              THE I/O AREA                                 
         L     RF,AIO1-L'AIO1(RF)                                               
         CLC   IOKEY(CT9AKSPR-CT9AKEY),0(RF)  SHOULD BE THE 9A PASSIVE          
         BNE   EXITL                          FOR THE SAME SOURCE               
*&&DO                                                                           
         CLI   0(RF),CT9AKTYQ                                                   
         BNE   EXITL                                                            
*&&                                                                             
         MVC   THIS.CT9ARECD(L'CT9AKEY),0(RF)                                   
         B     EXITOK                                                           
         DROP  THIS                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
*        CTGENRAD                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENRAD                                                       
         PRINT ON                                                               
*        DDCOMFACS                                                              
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*        CTMSGEQUS                                                              
         PRINT OFF                                                              
       ++INCLUDE CTMSGEQUS                                                      
         PRINT ON                                                               
*        DDFLDHDR                                                               
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008CTFIL28   04/19/18'                                      
         END                                                                    
