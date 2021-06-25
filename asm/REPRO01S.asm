*          DATA SET REPRO01S   AT LEVEL 018 AS OF 05/01/02                      
*&&      SET   NOP=N                                                            
*PHASE T80A01C                                                                  
*INCLUDE UNBOOK                                                                 
*INCLUDE RECUP                                                                  
T80A01   TITLE 'REPRO01 - ROUTINES FOR PROPOSAL PROGRAM'                        
PRO01    CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
***********************************************************************         
* BRANCH INDEX HELD IN HIGH ORDER BYTE OF RF                          *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
ROUT     NMOD1 RTWORKL,REPRO1**,R7,RR=R3,CLEAR=YES                              
         USING RTWORKD,RC                                                       
         ST    R1,RTPARMA                                                       
         MVC   RTPARMS,0(R1)                                                    
         ST    R3,RTRELO                                                        
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
*                                                                               
         SRL   RF,32-8                                                          
         CLM   RF,1,=AL1(ROUTSN)                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SLL   RF,2                                                             
         L     RF,ROUTS(RF)                                                     
         A     RF,RTRELO                                                        
         BR    RF                                                               
*                                                                               
ROUTS    DS    0F                                                               
         DC    A(VCON)                                                          
         DC    A(GCON)                                                          
         DC    A(VPRO)                                                          
         DC    A(VUPGD)                                                         
         DC    A(VDEMO)                                                         
         DC    A(AUNBOOK)                                                       
         DC    A(GETPARNT)                                                      
         DC    A(ARECUP)                                                        
         DC    A(MNIOINIT)                                                      
         DC    A(PACKTIME)                                                      
         DC    A(AFETCH)                                                        
         DC    A(PRBKFTCH)                                                      
         DC    A(DTLNFTCH)                                                      
         DC    A(SAVVAL1)                                                       
         DC    A(RESVAL1)                                                       
         DC    A(DTLNREFT)                                                      
         DC    A(DTLNDMFN)                                                      
         DC    A(GETPROF)                                                       
ROUTSN   EQU   (*-ROUTS)/4                                                      
*                                                                               
EXITL    MVI   BCDUB,0             SET CC LOW                                   
         B     EXITCC                                                           
EXITH    MVI   BCDUB,2             SET CC HIGH                                  
         B     EXITCC                                                           
EXITOK   MVI   BCDUB,1             SET CC EQUAL                                 
EXITCC   CLI   BCDUB,1                                                          
*                                                                               
EXIT     L     R1,RTPARMA          RETURN PARAMS TO CALLER                      
         MVC   0(L'RTPARMS,R1),RTPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
*                                                                               
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
*                                                                               
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
*                                                                               
EXITSLCK MVC   FVMSGNO,=AL2(GE$SLOCK)                                           
         B     EXITL               EXIT WITH SECURITY LOCKOUT                   
*                                                                               
EXITRNF  MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     EXITL               EXIT WITH RECORD NOT ON FILE                 
         EJECT                                                                  
***********************************************************************         
* VALIDATES CONTRACT FIELDS                                                     
*                                                                               
* WARNING: IO4 WILL GET CLOBBERED                                               
***********************************************************************         
VCON     GOTOX (GETCONQ,AREPRO01),BOPARM                                        
         BL    EXITL                                                            
***************                                                                 
* MARKET NAME                                                                   
***************                                                                 
         XC    IOKEY,IOKEY         GET MARKET NAME                              
         LA    R6,IOKEY                                                         
         USING RSTAREC,R6                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,CUAALF                                                  
         MVC   RSTAKSTA,CCONKSTA                                                
         L     R1,=AL4(XOREPDIR+XOHIGH+XIO4)                                    
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'RSTAKEY),IOKEYSAV                                        
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(INVSTA)      INVALID STATION                        
         B     EXITL                                                            
*                                                                               
         L     R1,=AL4(XOREPFIL+XOGET+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*&&NOP                                                                          
         L     R6,AIO4                                                          
         MVC   EMKTNAME,RSTAMKT    MARKET NAME                                  
         EDIT  (2,RSTACHAN),(4,ESTACHAN),ALIGN=LEFT,WRK=BOWORK1,       X        
               DUB=BODUB1                                                       
         MVC   ESTAAFFL,RSTAAFFL         AFFILIATE                              
*&&                                                                             
VCONST00 CLI   TWAACCS,C'$'        STATION IS USER                              
         BNE   VCONSTX                                                          
*                                                                               
         CLI   CSREC,R#PEND                                                     
         BNE   VCONSTX                                                          
*                                                                               
         L     R6,AIO4             CK STATION AUTH TO SEE CONTRACT              
         LA    R6,RSTAELEM-RSTARECD(R6)                                         
         USING RSTASOEL,R6         RSTASID IS VALID SIGN-ON FOR THIS            
         XR    RF,RF                                                            
VCONST10 CLI   0(R6),0                                                          
         BE    EXITSLCK            ERROR, SECURITY LOCKOUT                      
*                                                                               
         CLI   0(R6),X'06'                                                      
         BE    *+16                                                             
VCONST15 IC    RF,1(R6)                                                         
         LA    R6,0(RF,R6)                                                      
         B     VCONST10                                                         
*                                                                               
         CLC   CUUSER,RSTASID    STATION'S CONTRACTS                            
         BNE   VCONST15                                                         
         DROP  R6                                                               
*                                                                               
VCONSTX  DS    0H                                                               
***************                                                                 
* SALESPERSON NAME                                                              
***************                                                                 
VCONSA00 CLC   ESALNAME,SPACES                                                  
         BH    VCONSAX                                                          
*                                                                               
         XC    IOKEY,IOKEY         GET SALESPERSON NAME                         
         LA    R6,IOKEY                                                         
         USING RSALREC,R6                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,CUAALF                                                  
         MVC   RSALKSAL,CCONSAL                                                 
*                                                                               
         L     R1,=AL4(XOREPDIR+XOHIGH+XIO4)                                    
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'RSALKEY),IOKEYSAV                                        
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(INVAGY)      INVALID SALESPERSON                    
         B     EXITL                                                            
*                                                                               
         L     R1,=AL4(XOREPFIL+XOGET+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO4                                                          
         MVC   ESALNAME,RSALNAME                                                
         DROP  R6                                                               
*                                                                               
VCONSAX  DS    0H                                                               
***************                                                                 
* AGENCY NAME                                                                   
***************                                                                 
VCONAG00 CLC   EAGYNAM1,SPACES                                                  
         BH    VCONAGX                                                          
*                                                                               
         XC    IOKEY,IOKEY         GET AGENCY NAME                              
         LA    R6,IOKEY                                                         
         USING RAGYREC,R6                                                       
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY(6),CCONKAGY                                             
         MVC   RAGYKREP,CUAALF                                                  
*                                                                               
         L     R1,=AL4(XOREPDIR+XOHIGH+XIO4)                                    
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(RAGYKREP-RAGYKEY),IOKEYSAV                                 
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(INVAGY)      INVALID AGENCY                         
         B     EXITL                                                            
         CLC   IOKEY+25(2),=C'ZZ'                                               
         BE    VCONAG10                                                         
         CLC   IOKEY+25(2),IOKEYSAV+25                                          
         BE    VCONAG10                                                         
         MVC   IOKEY+25(2),=C'ZZ'                                               
*                                                                               
         L     R1,=AL4(XOREPDIR+XOHIGH+XIO4)                                    
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'RAGYKEY),IOKEYSAV                                        
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(INVAGY)      INVALID SALESPERSON                    
         B     EXITL                                                            
*                                                                               
VCONAG10 L     R1,=AL4(XOREPFIL+XOGET+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO4                                                          
         MVC   EAGYNAM1,RAGYNAM1                                                
*         MVC   EAGYNAM2,RAGYNAM2                                               
*         MVC   EAGYADD1,RAGYADD1                                               
*         MVC   EAGYADD2,RAGYADD2                                               
*         MVC   EAGYSTAT,RAGYSTAT                                               
*         MVC   EAGYZIP,RAGYZIP                                                 
         DROP  R6                                                               
*                                                                               
VCONAGX  DS    0H                                                               
***************                                                                 
* ADVERTISER NAME                                                               
***************                                                                 
VCONAV00 CLC   EADVNAME,SPACES                                                  
         BH    VCONAVX                                                          
*                                                                               
         XC    IOKEY,IOKEY         GET ADVERTISER NAME                          
         LA    R6,IOKEY                                                         
         USING RADVREC,R6                                                       
         MVI   RADVKTYP,X'08'                                                   
         MVC   RADVKADV,CCONKADV                                                
         MVC   RADVKREP,CUAALF                                                  
*                                                                               
         L     R1,=AL4(XOREPDIR+XOHIGH+XIO4)                                    
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(RADVKREP-RADVKEY),IOKEYSAV                                 
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(INVADV)      INVALID ADVERTISER                     
         B     EXITL                                                            
*                                                                               
         CLC   IOKEY+25(2),IOKEYSAV+25                                          
         BE    VCONAV10                                                         
         CLC   IOKEY+25(2),=C'ZZ'                                               
         BE    VCONAV10                                                         
         MVC   IOKEY+25(2),=C'ZZ'                                               
*                                                                               
         L     R1,=AL4(XOREPDIR+XOHIGH+XIO4)                                    
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'RADVKEY),IOKEYSAV                                        
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(INVADV)      INVALID ADVERTISER                     
         B     EXITL                                                            
*                                                                               
VCONAV10 L     R1,=AL4(XOREPFIL+XOGET+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO4                                                          
         MVC   EADVNAME,RADVNAME                                                
         DROP  R6                                                               
*                                                                               
VCONAVX  DS    0H                                                               
***************                                                                 
* PRODUCT NAME                                                                  
***************                                                                 
VCONPR00 CLC   EPRDNAME,SPACES                                                  
         BH    VCONPRX                                                          
*                                                                               
         XC    IOKEY,IOKEY         GET PRODUCT NAME                             
         LA    R6,IOKEY                                                         
         USING RPRDREC,R6                                                       
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKADV,CCONKADV                                                
         MVC   RPRDKPRD,CCONPRD                                                 
         MVC   RPRDKREP,CUAALF                                                  
*                                                                               
         L     R1,=AL4(XOREPDIR+XOHIGH+XIO4)                                    
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(RPRDKREP-RPRDKEY),IOKEYSAV                                 
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(INVPRD)      INVALID PRODUCT                        
         B     EXITL                                                            
*                                                                               
         CLC   IOKEY+25(2),CUAALF                                               
         BE    VCONPR10                                                         
         CLC   IOKEY+25(2),=C'ZZ'                                               
         BE    VCONPR10                                                         
         MVC   IOKEY+25(2),=C'ZZ'                                               
*                                                                               
         L     R1,=AL4(XOREPDIR+XOHIGH+XIO4)                                    
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'RPRDKEY),IOKEYSAV                                        
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(INVPRD)      INVALID PRODUCT                        
         B     EXITL                                                            
*                                                                               
VCONPR10 L     R1,=AL4(XOREPFIL+XOGET+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO4                                                          
         MVC   EPRDNAME,RPRDNAME                                                
         DROP  R6                                                               
VCONPRX  DS    0H                                                               
*****************                                                               
* DEV. SALESPERSON NAME                                                         
*****************                                                               
VCONDVS0 OC    CCONDVS,CCONDVS                                                  
         BZ    VCONDVSX                                                         
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R6,IOKEY                                                         
         USING RDSPKEY,R6                                                       
         MVI   RDSPKTYP,X'3A'                                                   
         MVC   RDSPKREP,CUAALF                                                  
         MVC   RDSPKSAL,CCONDVS                                                 
*                                                                               
         L     R1,=AL4(XOREPDIR+XOHIGH+XIO4)                                    
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'RDSPKEY),IOKEYSAV                                        
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(INVDVSAL)    INVALID DEV SALESPERSON                
         B     EXITL                                                            
*                                                                               
         L     R1,=AL4(XOREPFIL+XOGET+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO4                                                          
         MVC   EDVSNAME,RDSPNAME                                                
         DROP  R6                                                               
*                                                                               
VCONDVSX DS    0H                                                               
***************                                                                 
* LOOK UP REP RECORD                                                            
***************                                                                 
         XC    IOKEY,IOKEY         NOW LOOK UP REP RECORD                       
         LA    R6,IOKEY                                                         
         USING RREPREC,R6                                                       
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,CUAALF                                                  
*                                                                               
         L     R1,=AL4(XOREPDIR+XOHIGH+XIO4)                                    
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'RREPKEY),IOKEYSAV                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,=AL4(XOREPFIL+XOGET+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO4                                                          
         MVC   CPARREP,RREPPAR     SAVE PARENT REP                              
         DROP  R6                                                               
*                                                                               
VCONX    B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATES AND GETS CONTRACT RECORD                                            
***********************************************************************         
GCON     L     R2,0(R1)                                                         
*                                                                               
         LA    RF,CCONLEN                                                       
         XCEF  CCONNUM,(RF)            CLEAR ALL CONTRACT GLOBAL VALUES         
         LR    RE,RA                                                            
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         XC    0(L'SVCONNUM,RE),0(RE)  SAVED BTWN NTRSES, BUT NOTHING           
*                                                                               
         SR    R0,R0                                                            
         CLI   FVILEN,0                                                         
         BE    EXITNO              NO INPUT                                     
         TM    FVIIND,FVINUM       VALID NUMERIC?                               
         BZ    EXITNOTN            NO, ERROR                                    
*                                                                               
         ZIC   R1,FVXLEN           PACK THE CONTRACT NUMBER                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  BODUB1,FVIFLD(0)                                                 
         CVB   R0,BODUB1                                                        
*                                                                               
         ZAP   BOWORK1+10(5),=P'0'                                              
         SP    BOWORK1+10(5),BODUB1+3(5)                                        
         MVO   BOWORK1(5),BOWORK1+10(5) CONTRACT NUM IN PWOS                    
         MVC   CCONKNUM,BOWORK1                                                 
*                                                                               
         ZAP   BOWORK1+10(5),=P'99999999'                                       
         SP    BOWORK1+10(5),BODUB1+3(5)                                        
         MVO   BOWORK1(5),BOWORK1+10(5) CONTRACT NUM IN 9'S COMPLEMENT          
*                                                                               
         MVC   CCONNUM,BOWORK1                                                  
         LR    RE,RA                                                            
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         MVC   0(L'SVCONNUM,RE),BOWORK1    THIS IS SAVED BTWN NTRSES            
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R6,IOKEY                                                         
         USING RCONPTYP,R6                                                      
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,CUAALF                                                  
         MVC   RCONPCON,BOWORK1    CONTRACT NUMBER                              
         DROP  R6                                                               
*                                                                               
         L     R1,=AL4(XOREPDIR+XOHIGH+XIO3)                                    
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'RCONKEY),IOKEYSAV                                        
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     EXITL                                                            
*                                                                               
         CLI   IOERR,IOEDEL            IS KEY MARKED FOR DELETION?              
         BNE   *+14                                                             
GCON10   MVC   FVMSGNO,=AL2(FVFRDEL)   YES                                      
         B     EXITL                                                            
*                                                                               
         L     R1,=AL4(XOREPFIL+XOGET+XIO3)                                     
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO3                                                          
         USING RCONKEY,R6                                                       
*                                                                               
         TM    RCONCNTL,X'80'      RECORD MARKED FOR DELETION?                  
         BNZ   GCON10              YES                                          
*                                                                               
         CLI   ASONOFF,ASOFF       RUNNING OFFLINE?                             
         BE    GCON20                                                           
         TM    CUSTAT,CUSDDS       DDS TERMINAL?                                
         BNZ   GCON20                                                           
         CLC   =C'O=',CUACCS       TEST FOR OFFICE RESTRICTION                  
         BNE   GCON20                                                           
         TM    CUAUTH,X'80'        TEST IF TERMINAL ALLOWED ACCESS              
         BO    GCON20              TO ALL OFFICES                               
         CLC   RCONKOFF,CUACCS+2   ELSE,COMPARE OFFICES                         
         BNE   EXITSLCK                                                         
*                                                                               
GCON20   MVC   CCONKSTA,RCONKSTA   STATION CALL LETTERS                         
         MVC   ESTATION,SPACES                                                  
         MVC   ESTATION(4),RCONKSTA ALSO SAVE IN PRINTABLE FORMAT               
         CLI   RCONKSTA+4,C' '     TV                                           
         BE    GCON30                                                           
         LA    RE,ESTATION+3       FOR RADIO, SHOW -A,-F,-C                     
         CLI   0(RE),C' '                                                       
         BE    *+8                                                              
         LA    RE,1(RE)                                                         
         MVI   0(RE),C'-'                                                       
         MVC   1(1,RE),RCONKSTA+4                                               
*                                                                               
GCON30   MVC   CCONKAGY(6),RCONKAGY  AGENCY CODE                                
         MVC   CCONKADV,RCONKADV   ADVERTISER CODE                              
         MVC   CCONKOFF,RCONKOFF                                                
         MVC   CCONKGRP,RCONKGRP   STATION GROUP/SUBGROUP                       
*                                                                               
         OI    CCONFLG1,CCONIPND   ASSUME CONTRACT IS PENDING                   
         LA    R6,RCONELEM                                                      
         USING RCONELEM,R6                                                      
         MVC   CCONSAL,RCONSAL     SALESPERSON CODE                             
         MVC   CCONTEM,RCONTEM     SALES TEAM                                   
         MVC   CCONDAT,RCONDATE    START/END DATES                              
         MVC   CSOURCE,RCONRTGS    RATING SERVICE                               
         MVC   ECONBUYR,RCONBUYR   BUYER NAME                                   
         MVC   CCONWKS,RCONWKS     NUMBER OF WEEKS IN CONTRACT                  
         MVC   CCONCTGY,RCONCTGY   CATEGORY                                     
         MVC   CCONTYPE,RCONTYPE   TYPE                                         
*                                                                               
         TM    RCONMODR,X'10'      BUY LINE ADDED?                              
         BZ    *+8                 YES                                          
         NI    CCONFLG1,X'FF'-CCONIPND   CONTRACT IS NOT PENDING                
*                                                                               
         CLC   RCONPRD,SPACES                                                   
         BE    *+14                                                             
         MVC   CCONPRD,RCONPRD                                                  
         B     GCON40                                                           
         DROP  R6                                                               
*                                                                               
         L     R6,AIO3                                                          
         LA    R6,RCONELEM-RCONKEY(R6)                                          
         MVI   RTELCODE,X'05'      GET PRODUCT NAME                             
         BAS   RE,FIRSTEL                                                       
         USING RCONEXEL,R6                                                      
         MVC   EPRDNAME,RCONEXPR                                                
         DROP  R6                                                               
*                                                                               
GCON40   L     R6,AIO3                                                          
         LA    R6,RCONELEM-RCONKEY(R6)                                          
         MVI   RTELCODE,X'03'      BUCKETS?                                     
         BAS   RE,FIRSTEL                                                       
         BNE   *+8                 NO                                           
         NI    CCONFLG1,X'FF'-CCONIPND   CONTRACT IS NOT PENDING                
*                                                                               
         L     R6,AIO3                                                          
         LA    R6,RCONELEM-RCONKEY(R6)                                          
         MVI   RTELCODE,X'04'      STATION CONTRACT AMOUNT?                     
         BAS   RE,FIRSTEL                                                       
         BNE   *+8                 NO                                           
         NI    CCONFLG1,X'FF'-CCONIPND   CONTRACT IS NOT PENDING                
*                                                                               
         L     R6,AIO3                                                          
         LA    R6,RCONELEM-RCONKEY(R6)                                          
         MVI   RTELCODE,X'06'      SPL/EPL?                                     
         BAS   RE,FIRSTEL                                                       
         BNE   *+8                 NO                                           
         NI    CCONFLG1,X'FF'-CCONIPND   CONTRACT IS NOT PENDING                
*                                                                               
         L     R6,AIO3                                                          
         LA    R6,RCONELEM-RCONKEY(R6)                                          
         MVI   RTELCODE,X'07'      SPL COMMENTS?                                
         BAS   RE,FIRSTEL                                                       
         BNE   *+8                 NO                                           
         NI    CCONFLG1,X'FF'-CCONIPND   CONTRACT IS NOT PENDING                
         NI    CCONFLG1,X'FF'-CCONSARQ   NO SAR ELEMENT YET                     
*                                                                               
         L     R6,AIO3                                                          
         LA    R6,RCONELEM-RCONKEY(R6)                                          
         MVI   RTELCODE,X'12'      GET SAR ELEMENT (IF ANY)                     
         BAS   RE,FIRSTEL               -SAVE BOOKS, DEMOS, LENGTHS             
         BNE   GCON50                                                           
*                                                                               
         OI    CCONFLG1,CCONSARQ     FOUND SAR ELEMENT                          
*                                                                               
         CLI   1(R6),RSARXLTH      MAKE SURE ITS AN EXPAND ELEMENT              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL                                                            
*                                                                               
         USING RSARXEL,R6                                                       
         MVC   CSARLEN,RSARXRFR                                                 
         MVC   CSARXBGT,RSARXBGT                                                
         MVC   CSARXFLG,RSARXFLG                                                
         TM    RSARXFLG,X'18'      FORCAST?                                     
         BZ    *+8                 YES                                          
         NI    CCONFLG1,X'FF'-CCONIPND   CONTRACT IS NOT PENDING                
         MVC   CSARXSHG,RSARXSHG                                                
         MVC   CSARXFL2,RSARXFL2                                                
*                                                                               
         TM    RSARXFLG,X'04'      PROPOSAL EXPANSION USED?                     
         BO    GCON44              YES                                          
         SPACE 1                                                                
*-------------------------------------                                          
* COPY INFORMATION FROM SARX ELEMENT                                            
*--------------------------------------                                         
         CLC   RSARXBKS(2),=C'DR'  FOR DIRECT RESPONSE - SKIP BOOKS             
         BE    GCON42              AND DEMOS                                    
*                                                                               
         CLC   RSARXBKS(2),=C'PP'  FOR PAID PROGRAMING - SKIP BOOKS             
         BE    GCON42              AND DEMOS                                    
*                                                                               
         LA    R0,6                NUMBER OF BOOKS TO COPY                      
         LA    RE,RSARXBKS                                                      
         LA    RF,CSARBKS                                                       
         MVC   2(3,RF),0(RE)       MOVE BOOK                                    
         LA    RF,5(RF)                                                         
         LA    RE,3(RE)                                                         
         BCT   R0,*-14                                                          
         MVC   CSARDEM,RSARXDEM    MOVE DEOMS                                   
*                                                                               
GCON42   DS    0H                                                               
         LA    R0,6                NUMBER OF DAYPARTS                           
         LA    RE,RSARXDPT                                                      
         LA    RF,CSARDPT                                                       
         MVC   0(1,RF),0(RE)       MOVE DAYPART                                 
         MVC   3(2,RF),1(RE)       MOVE CPP                                     
         LA    RF,5(RF)                                                         
         LA    RE,3(RE)                                                         
         BCT   R0,*-20                                                          
*                                                                               
         OI    CCONFLG1,CCONDPMQ   OLD CONTRACT MUST USE TABLE                  
         B     GCON50                                                           
         SPACE 1                                                                
*--------------------------------------------------                             
* COPY INFORMATION FROM PROPOSAL EXPANSION ELEMENTS                             
*--------------------------------------------------                             
GCON44   CLC   RSARXBKS(2),=C'DR'  FOR DIRECT RESPONSE - SKIP BOOKS             
         BE    GCON46              AND DEMOS                                    
         CLC   RSARXBKS(2),=C'PP'  FOR PAID PROGRAMING - SKIP BOOKS             
         BE    GCON46              AND DEMOS                                    
         MVC   CSARDEM,RSARXDEM    MOVE DEMOS                                   
*                                                                               
         L     R6,AIO3                                                          
         LA    R6,RCONELEM-RCONKEY(R6)                                          
         MVI   RTELCODE,RCPRBKEQ   GET PROPOSAL EXP BOOK ELEMENT                
         USING RCPRBKEL,R6                                                      
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                SHOULD BE HERE                               
         SR    R0,R0                                                            
         ZIC   R1,RCPRBKLN                                                      
         SH    R1,=AL2(RCPRBKOQ)                                                
         SR    RE,RE                                                            
         LA    RE,L'RCPRBKBK                                                    
         DR    R0,RE                                                            
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,RCPRBKBK                                                      
         LA    RF,CSARBKS                                                       
         MVC   0(L'RCPRBKBK,RF),0(RE)   MOVE BOOK OR LABEL                      
         LA    RF,L'RCPRBKBK(RF)                                                
         LA    RE,L'RCPRBKBK(RE)                                                
         BCT   R1,*-14                                                          
*                                                                               
GCON46   DS    0H                                                               
         L     R6,AIO3                                                          
         LA    R6,RCONELEM-RCONKEY(R6)                                          
         MVI   RTELCODE,RCPRDPEQ   GET PROPOSAL EXP DAYPART/CPP ELEM            
         USING RCPRDPEL,R6                                                      
         BAS   RE,FIRSTEL                                                       
         BNE   GCON50                                                           
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,RCPRDPLN                                                      
         SH    R1,=AL2(RCPRDPOX)                                                
         SR    RE,RE                                                            
         LA    RE,L'RCPRDPDP                                                    
         DR    R0,RE                                                            
         LTR   R1,R1                                                            
         BZ    GCON50                                                           
*                                                                               
         LTR   R0,R0               OLD ELEMENT?                                 
         BNZ   GCON47              NO                                           
         OI    CCONFLG1,CCONDPMQ   OLD ELEMENTS USE HARDCODED TABLE             
         LA    RE,RCPRDPDP-(RCPRDPOQ-RCPRDPOX)                                  
         B     GCON48                                                           
*                                                                               
GCON47   LA    RE,RCPRDPDP                                                      
         TM    RCPRDPFL,RCPRDPMQ   USING DAYPART MENU?                          
         BO    GCON48              YES                                          
         OI    CCONFLG1,CCONDPMQ   NO - USES HARDCODED TABLE                    
*                                                                               
         LA    RE,RCPRDPDP                                                      
GCON48   LA    RF,CSARDPT                                                       
         MVC   0(L'RCPRDPDP,RF),0(RE)   MOVE DAYPART & CPP                      
         LA    RF,L'RCPRDPDP(RF)                                                
         LA    RE,L'RCPRDPDP(RE)                                                
         BCT   R1,*-14                                                          
*                                                                               
**************                                                                  
* GET THE DEVELOPMENTAL SALESPERSON & CONTRACT TYPE CODES                       
**************                                                                  
GCON50   DS    0H                                                               
         L     R6,AIO3                                                          
         LA    R6,RCONELEM-RCONKEY(R6)                                          
         MVI   RTELCODE,X'18'      GET THE DEVELOPMENTAL/INVOICE ELEM           
         USING RCONDVEL,R6                                                      
         BAS   RE,FIRSTEL                                                       
         BNE   GCON60                                                           
         MVC   CCONDVS,RCONDVSP    DEV SALESPERSON                              
         MVC   CCONDVT,RCONDVCT    DEV CONTRACT TYPE                            
*                                                                               
GCON60   DS    0H                                                               
         DROP  R6                                                               
GCONX    B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATES THE PROPOSAL NUMBER IN FVIFLD                                       
***********************************************************************         
VPRO     DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(SVPRONUM-TWAD)                                             
         XC    0(L'SVPRONUM,RE),0(RE)  SAVED BTWN NTRSES, BUT NOTHING           
*                                                                               
         CLI   FVILEN,0                                                         
         BE    EXITNO              NO INPUT                                     
*                                                                               
         TM    FVIIND,FVINUM       BETTER BE NUMERIC                            
         BZ    EXITNV                                                           
*                                                                               
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  BODUB1,FVIFLD(0)                                                 
         CVB   R0,BODUB1                                                        
*                                                                               
         CH    R0,=H'255'          ERROR IF MORE THAN 255 PROPOSALS             
         BH    EXITNV                  PER CONTRACT                             
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING RPROKEY,R2                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,CCONNUM                                                 
         STC   R0,RPROKPRO                                                      
         XI    RPROKPRO,FF                                                      
*                                                                               
         L     R1,=AL4(XOREPDIR+XOHID+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
*                                                                               
         CLC   IOKEY(L'RPROKMST),IOKEYSAV         RECORD EXISTS?                
         BNE   EXITRNF                            NO                            
*                                                                               
         CLI   CSREC,R#PRO                                                      
         BNE   *+12                                                             
         CLI   CSACT,18                           PRO/RESTORE                   
         BE    VPRO2                                                            
*                                                                               
         TM    IOKEY+(RPROKCTL-RPROKEY),X'80'     DELETED?                      
         BZ    VPRO4                              NO                            
         MVC   FVMSGNO,=AL2(FVFRDEL)                                            
         B     EXITL                                                            
*                                                                               
VPRO2    TM    IOKEY+(RPROKCTL-RPROKEY),X'80'     DELETED?                      
         BNZ   VPRO4                              YES                           
         MVC   FVMSGNO,=AL2(FVFMIXI)                                            
         B     EXITL                                                            
*                                                                               
VPRO4    CLI   TWAACCS,C'$'        STATION IS USER                              
         BNE   VPROX                                                            
*                                                                               
         L     R1,=AL4(XOREPFIL+XOGET+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO4                                                          
         LA    R6,RPROR1ST-RPROHDRD(R6)                                         
         MVI   RTELCODE,X'01'                                                   
         USING RPRDSELD,R6                                                      
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    RPRDSFLG,RPRDSSTA   SATION ALLOWED?                              
         BZ    EXITSLCK            ERROR, SECURITY LOCKOUT                      
*                                                                               
VPROX    DS    0H                                                               
         MVC   BPRONUM,RPROKPRO                   THE PROPOSAL NUMBER           
         LR    RE,RA                                                            
         AH    RE,=Y(SVPRONUM-TWAD)                                             
         MVC   0(L'SVPRONUM,RE),BPRONUM                                         
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE UPGRADE EXPRESSION                                        
*                                                                               
* ON ENTRY:    PARAM 1  BYTE  0    ALLOWANCE BYTE                               
*                                   X'80' - UPGRADE EXPRESSION OKAY             
*                                   X'40' - SHARE BOOK EXPRESSION OKAY          
*                                   X'20' - DAY/TIME OVERRIDE EXPR OKAY         
*                                   X'10' - PUT AVERAGING EXPRESSION OK         
*                                   X'08' - SHR AVERAGING EXPRESSION OK         
*                       BYTES 1-3  A(FIELD HEADER OF UPGRADE FIELD)             
*                                                                               
* ON EXIT:     PARAM 1  BYTE 0     WHICH FIELDS WERE INPUT AS ABOVE             
*                                                                               
*              BOWORK1+00  1 BYTE   UPGRADE FILE                                
*              BOWORK1+01 12 BYTES UPGRADE EXPRESSION                           
*              BOWORK1+13  3 BYTES SHARE BOOK (X'00'S - LATEST)                 
*              BOWORK1+16  1 BYTE  OVERRIDE DAY(S)                              
*              BOWORK1+17  4 BYTES OVERRIDE TIME(S)                             
*              BOWORK1+21  1 BYTE  PUT AVG VALUE (CHAR 1 OR 2)                  
*              BOWORK1+22  1 BYTE  SHR AVG VALUE (CHAR 1 OR 2)                  
*              BOWORK1+23  6 BYTES SHARE BOOK LIST (MAX OF 3)                   
*              BOWORK1+29  1 BYTE  SPECIAL BOOK TYPE                            
*                                                                               
* WARNING: BOWORK1, AIO5, AND BOBYTE1 GET CLOBBERED                             
***********************************************************************         
VUPGD    DS    0H                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
         MVC   BOBYTE1,0(R1)       SAVE ALLOWANCE BYTE                          
         L     R2,0(R1)            R2 = A(FIELD HEADER)                         
         LA    R2,0(R2)                                                         
*                                                                               
         XC    BOWORK1(32),BOWORK1                                              
*                                                                               
         CLC   =C'UPT=BK/',8(R2)   FORMAT  UPT=BK/MYY(/MYY/MYY/MYY) ??          
         BNE   VUPGD10                                                          
*                                                                               
         ZIC   RE,5(R2)            YES, ALTER FORMAT TO                         
         SH    RE,=H'5'                    UPT=IX/100,BK=MYY(/MYY/....)         
         BM    VUPGDINV                                                         
*                                                                               
         MVC   BOELEM(7),=C'IX/100,'                                            
         EX    RE,*+4                                                           
         MVC   BOELEM+7(0),8+4(R2)                                              
         MVI   BOELEM+9,C'='       CHANGE THE 'BK/' TO 'BK='                    
         LA    RE,7(RE)                                                         
         EX    RE,*+4                                                           
         MVC   8+4(0,R2),BOELEM                                                 
         LA    RE,5(RE)                                                         
         STC   RE,5(R2)                                                         
         LA    RE,8(RE)                                                         
         STC   RE,0(R2)                                                         
*                                                                               
VUPGD10  GOTO1 VSCANNER,BODMCB,(30,(R2)),AIO5,C',=,='                           
         MVC   RTBYTE2,4(R1)    SAVE NUMBER OF SCANNER LINES                    
         MVI   RTBYTE3,0           FIELD INPUT INDICATORS                       
*                                                                               
         CLI   RTBYTE2,0                                                        
         BE    VUPGDINV                                                         
         L     R6,AIO5             R6 = A(SCANNER BLOCK)                        
         MVI   RTBYTE4,1                                                        
*                                                                               
VUPGD20  CLI   0(R6),0             TEST L'FIRST HALF OF ENTRY                   
         BE    VUPGDINV                                                         
*                                                                               
         CLI   0(R6),L'UPGNAME                                                  
         BH    VUPGDINV                                                         
*                                                                               
         CLI   1(R6),0             TEST L'SECOND HALF OF ENTRY                  
         BE    VUPGDINV                                                         
*                                                                               
         ZIC   R1,0(R6)                                                         
         BCTR  R1,0                R1=L'FIRST HALF OF INPUT-1                   
         LA    R3,UPGTAB                                                        
         USING UPGTABD,R3          R3=A(UPGRADE TABLE)                          
*                                                                               
VUPGD30  CLI   UPGNAME,0           TEST E-O-T                                   
         BE    VUPGDINV                                                         
*                                                                               
         CLI   UPGSHRT,C' '        TEST ENTRY HAS SHORT KEYWORD                 
         BE    VUPGD40                                                          
*                                                                               
         LA    RE,2                ADJUST RE (LENGTH) FOR EX                    
         CLI   UPGSHRT+2,C' '                                                   
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         CLI   UPGSHRT+1,C' '                                                   
         BNE   *+6                                                              
         BCTR  RE,0                                                             
*                                                                               
         CR    R1,RE               TEST INPUT LEN = SHORT NAME LEN              
         BNE   VUPGD40                                                          
*                                                                               
         EX    R1,*+8              YES - MATCH ON SHORT NAME                    
         B     *+10                                                             
         CLC   UPGSHRT(0),12(R6)                                                
         BE    VUPGD60                                                          
*                                                                               
VUPGD40  EX    R1,*+8              MATCH ON LONG NAME                           
         B     *+10                                                             
         CLC   UPGNAME(0),12(R6)                                                
         BE    VUPGD60                                                          
*                                                                               
VUPGD50  LA    R3,UPGTABL(R3)      BUMP TO NEXT UPGRADE TABLE ENTRY             
         B     VUPGD30                                                          
*                                                                               
VUPGD60  MVC   BODUB1(1),RTBYTE3   TEST KEYWORD INPUT PREVIOUSLY                
         NC    BODUB1(1),UPGIND1                                                
         BZ    *+14                                                             
         MVC   FVMSGNO,=Y(DPNTRYNA)   DUPLICATE ENTRY NOT ALLOWED               
         B     EXITL                                                            
*                                                                               
         MVC   BODUB1(1),BOBYTE1     TEST KEYWORD IS VALID                      
         NC    BODUB1(1),UPGIND1                                                
         BZ    VUPGDINV                                                         
*                                                                               
         XC    BOELEM,BOELEM                                                    
         ZIC   R1,1(R6)                                                         
         EX    R1,*+8              DON'T CARE IF WE COPY ONE MORE BYTE          
         B     *+10                                                             
         MVC   BOELEM+8(0),22(R6)                                               
         STC   R1,BOELEM+5                                                      
         LA    R1,8(R1)                                                         
         STC   R1,BOELEM                                                        
*                                                                               
         OC    RTBYTE3,UPGIND1     SET THIS KEYWORD INPUT                       
         SR    RF,RF                                                            
         ICM   RF,3,UPGROUT                                                     
         LA    RF,PRO01(RF)        RF=A(VALIDATION ROUTINE)                     
*                                                                               
         BASR  RE,RF                                                            
         BNE   VUPGDNO                                                          
*                                                                               
         ZIC   R1,RTBYTE4          ANY MORE SCANNER ENTRIES?                    
         LA    R1,1(R1)                                                         
         CLM   R1,1,RTBYTE2                                                     
         BH    VUPGDYES            NO                                           
*                                                                               
         STC   R1,RTBYTE4          YES                                          
         LA    R6,22+30(R6)        BUMP TO NEXT SCANNER TABLE ENTRY             
         B     VUPGD20                                                          
*                                                                               
VUPGDYES MVC   BODMCB(1),RTBYTE3   RETURN FIELDS INPUT                          
         B     EXITOK                                                           
*                                                                               
VUPGDINV MVC   FVMSGNO,=Y(INVUPGRD)   INVALID UPGRADE FORMULA                   
VUPGDNO  B     EXITL                                                            
*                                                                               
UPGTAB   DS    0H               ** UPGRADE VALIDATION TABLE **                  
         DC    C'PUT     ',C'   ',X'1000',AL2(VALUPA-PRO01)                     
         DC    C'SHR     ',C'   ',X'0800',AL2(VALUPA-PRO01)                     
         DC    C'RTG     ',C'   ',X'0800',AL2(VALUPA-PRO01)                     
         DC    C'RP      ',C'   ',X'1800',AL2(VALUPA-PRO01)                     
         DC    C'TUPGRADE',C'UPT',X'8000',AL2(VALUPE-PRO01)                     
         DC    C'PUPGRADE',C'UPP',X'8000',AL2(VALUPE-PRO01)                     
         DC    C'BOOK    ',C'BK ',X'4000',AL2(VALUPB-PRO01)                     
         DC    C'DAYTIME ',C'DT ',X'2000',AL2(VALUPD-PRO01)                     
UPGTABX  DC    X'00'                                                            
*                                                                               
UPGTABD  DSECT                                                                  
UPGNAME  DS    CL8                 KEYWORD NAME                                 
UPGSHRT  DS    CL3                 SHORT KEYWORD NAME                           
UPGIND1  DS    XL1                 INDICATORS                                   
UPGIND2  DS    XL1                 INDICATORS                                   
UPGROUT  DS    AL2                 DISPLACEMENT OF VALIDATION ROUTINE           
UPGTABL  EQU   *-UPGTABD                                                        
*                                                                               
PRO01    CSECT                                                                  
         DS    0H                                                               
***********************************                                             
* VALIDATE UPGRADE EXPRESSION                                                   
***********************************                                             
VALUPE   LR    R0,RE                                                            
         MVC   BOWORK1(1),UPGSHRT+2 SET FILE INDICATOR                          
         GOTO1 VUPVAL,BODMCB,BOELEM,(C'Y',BOWORK1+32),(C'/',ACOM)               
         MVC   BOWORK1+1(12),BOWORK1+34   UPGRD EXPR FROM UPGRADE ELEM          
VALUP05D USING RAVLNBKS,BOWORK1+1   X'05' ELEM WITHOUT ELCODE AND ELLEN         
         CLI   0(R1),0                                                          
         BNE   *+10                                                             
         MVC   FVMSGNO,=Y(INVUPGRD)                                             
         B     VALUPX                                                           
***********************************                                             
* VALIDATE OVERRIDE BOOK                                                        
***********************************                                             
VALUPB   LR    R0,RE                                                            
         ZIC   RF,BOELEM+5                                                      
         LA    R1,BOELEM+8                                                      
*                                                                               
VALUPB2  CLI   0(R1),C'/'          ALTER SLASHES TO COMMAS IF THEY              
         BNE   VALUPB4             SEPARATE BOOKS                               
         CLI   1(R1),C'0'                                                       
         BNL   VALUPB4                                                          
         MVI   0(R1),C','                                                       
VALUPB4  LA    R1,1(R1)                                                         
         BCT   RF,VALUPB2                                                       
*                                                                               
         GOTO1 VBOOKVAL,BODMCB,(C'N',BOELEM),(4,BOWORK1+32),           X        
               (C'B',VSCANNER),BODUB1                                           
         SR    RF,RF                                                            
         ICM   RF,1,4(R1)          RF=N'BOOKS                                   
         BZ    VALUPB9                                                          
*                                                                               
         TM    BOWORK1+32,X'BF'    TEST FOR FUNNY BOOK FORMATS                  
         BNZ   VALUPB9                                                          
         MVC   BOWORK1+13(3),BOWORK1+32   COPY OVERRIDE BOOK                    
         CLI   BODUB1,C' '                SPECIAL BOOK?                         
         BE    *+10                       NO                                    
         MVC   BOWORK1+29(1),BODUB1         COPY BK='S  BOOK TYPE               
*                                                                               
         CLI   VALUP05D.RAVLNBT,0       ANY BOOK TYPE IN UPGRADE?               
         BE    VALUPB5                  NO                                      
         CLI   BODUB1,C' '              IS BOOK TYPE A SPACE?                   
         BE    VALUPB5                  YES, IGNORE                             
VALUPB4A CLC   VALUP05D.RAVLNBT,BODUB1  SAME BOOK TYPE?                         
         BNE   VALUPB9                  NO, ERROR                               
*                                                                               
VALUPB5  CLI   4(R1),1             TEST 2 OR MORE BOOKS                         
         BNH   VALUPX                                                           
         CLI   4(R1),4                                                          
         BH    VALUPB9                                                          
         BCTR  RF,0                                                             
         LA    R1,BOWORK1+23                                                    
         LA    RE,BOWORK1+32+3                                                  
VALUPB6  TM    0(RE),X'BE'                                                      
         BNZ   VALUPB9                                                          
         MVC   0(2,R1),1(RE)                                                    
         LA    R1,2(R1)                                                         
         LA    RE,3(RE)                                                         
         BCT   RF,VALUPB6                                                       
         B     VALUPX                                                           
*                                                                               
VALUPB9  MVC   FVMSGNO,=Y(INVBKEXP)    INVALID BOOK EXPRESSION                  
         B     VALUPX                                                           
***********************************                                             
* VALIDATE DAY/TIME OVERRIDE                                                    
***********************************                                             
VALUPD   LR    R0,RE                                                            
         GOTO1 VSCANNER,BODMCB,BOELEM,(2,BOWORK1+32),C',=,/'                    
         CLI   4(R1),1                                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=Y(INVUPGRD)                                             
         B     VALUPX                                                           
*                                                                               
         GOTO1 VDAYVAL,BODMCB,(BOWORK1+32,BOWORK1+32+12),BODUB1,       X        
               BODUB1+1                                                         
         CLI   BODUB1,0                                                         
         BE    VALUPD2                                                          
         MVC   BOWORK1+16(1),BODUB1                                             
         GOTO1 VTIMVAL,BODMCB,(BOWORK1+32+1,BOWORK1+32+22),BODUB1               
         CLI   0(R1),X'FF'                                                      
         BE    VALUPD4                                                          
         MVC   BOWORK1+17(4),BODUB1                                             
         B     VALUPX                                                           
*                                                                               
VALUPD2  MVC   FVMSGNO,=Y(INVDAYFD)                                             
         B     VALUPX                                                           
*                                                                               
VALUPD4  MVC   FVMSGNO,=Y(INVTIMFD)                                             
         B     VALUPX                                                           
***********************************                                             
* VALIDATE PUT/SHR AVERAGING                                                    
***********************************                                             
VALUPA   LR    R0,RE                                                            
         CLI   BOELEM+5,1                                                       
         BNE   VALUPA2                                                          
*                                                                               
         CLI   BOELEM+8,C'1'                                                    
         BE    *+12                                                             
         CLI   BOELEM+8,C'2'                                                    
         BNE   VALUPA2                                                          
*                                                                               
         TM    UPGIND1,X'10'                                                    
         BZ    *+10                                                             
         MVC   BOWORK1+21(1),BOELEM+8                                           
*                                                                               
         TM    UPGIND1,X'08'                                                    
         BZ    *+10                                                             
         MVC   BOWORK1+22(1),BOELEM+8                                           
         B     VALUPX                                                           
*                                                                               
VALUPA2  MVC   FVMSGNO,=Y(INVUPGRD)                                             
         B     VALUPX                                                           
*                                                                               
VALUPX   LR    RE,R0                                                            
         CLC   FVMSGNO,=Y(FVFOK)                                                
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE DEMO(S)                                                              
*                                                                               
* ON ENTRY:    PARAM 1  BYTE  0    C'Y' = VALIDATE CPM/CPP/PRIMARY DEMO         
*                       BYTES 1-3  A(DEMO FIELD HEADER)                         
*                                                                               
*              PARAM 2             A(DEMO AREA TO FILL)                         
*                                                                               
* ON EXIT:     PARAM 2  BYTE  0    # OF DEMOS VALIDATED                         
*                       BYTES 1-3  A(FILLED IN DEMO AREA)                       
*                                                                               
* NOTE: AIO5 GETS CLOBBERED                                                     
***********************************************************************         
VDEMO    DS    0H                                                               
         MVC   RTBYTE2,0(R1)       C'Y' = VALIDATE CPM/CPP/PRIMARY DEMO         
         L     R2,0(R1)            R2 = A(DEMO FIELD HEADER)                    
         LA    R2,0(R2)                                                         
         L     R6,4(R1)            R6 = A(DEMO AREA TO FILL)                    
         MVC   RTBYTE3,4(R1)                                                    
*                                                                               
         L     RE,AIO4             CLEAR FOR PARSNIP                            
         LA    RF,480                                                           
         XCEFL                                                                  
*                                                                               
         GOTO1 VPARSNIP,BODMCB,(FVILEN,FVIFLD),(RTBYTE3,AIO4),0                 
         CLI   4(R1),1                                                          
         BL    EXITNV                                                           
         CLC   4(1,R1),RTBYTE3                                                  
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(TOOMNYDM)   TOO MANY BOOKS                          
         B     EXITL                                                            
*                                                                               
         L     R3,AIO5                                                          
         USING DBLOCKD,R3                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOM                                                    
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'PAV'                                                   
         DROP  R3                                                               
*                                                                               
         GOTO1 VDEMOVAL,BODMCB,(R2),(RTBYTE3,(R6)),(RTBYTE2,(R3))               
         CLI   4(R1),0                                                          
         BNE   VDMOX                                                            
*                                                                               
         LA    RE,8(R2)                                                         
         CLI   0(R1),1                                                          
         BNH   VDMO30                                                           
         ZIC   RF,0(R1)            GET THE # OF DEMO IN ERROR                   
         B     VDMO20                                                           
*                                                                               
VDMO10   CLI   0(RE),C','          FIND DEMO IN ERROR                           
         BE    VDMO20                                                           
         LA    RE,1(RE)                                                         
         B     VDMO10                                                           
*                                                                               
VDMO20   LA    RE,1(RE)            POINT AFTER THE COMMA                        
         BCT   RF,VDMO10                                                        
*                                                                               
VDMO30   LA    R0,8(R2)            SO WE CAN POINT TO IT ON ERROR               
         SR    RE,R0                                                            
         STC   RE,FVERRNDX                                                      
         MVC   FVMSGNO,=Y(INVDMEXP)                                             
         B     EXITL                                                            
*                                                                               
VDMOX    B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAYS DEMO(S)                                                              
*                                                                               
* ON ENTRY:    PARAM 1              A(DEMOS LIST)                               
*              (R2)                 A(FIELD HEADER)                             
***********************************************************************         
DDEMO    L     R3,0(R1)                                                         
         ZIC   R4,MAX                                                           
*                                                                               
DDMO10   CLI   1(R3),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R3),C'I'                                                       
         LA    R3,3(R3)                                                         
         BCT   R4,DDMO10                                                        
*                                                                               
         L     R3,0(R1)                                                         
         L     R4,AIO5                                                          
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBCOMFCS,ACOM                                                    
         GOTO1 VDEMOCON,BODMCB,(MAX,0(R3)),(9,8(R2)),(0,DBLOCK)                 
         DROP  R4                                                               
*                                                                               
DDMOX    B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* UNBOOK AVAILABLE TO ALL OVERLAYS                                              
***********************************************************************         
AUNBOOK  DS    0H                                                               
         L     RF,=V(UNBOOK)                                                    
         A     RF,RTRELO                                                        
         GOTO1 (RF),RTPARMS                                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* RECUP AVAILABLE TO ALL OVERLAYS                                               
***********************************************************************         
ARECUP   DS    0H                                                               
         L     RF,=V(RECUP)                                                     
         A     RF,RTRELO                                                        
         GOTO1 (RF),RTPARMS                                                     
         B     EXITOK                                                           
***********************************************************************         
* FETCH DEMO/SHARE/LEVEL VALUES                                                 
*                                                                               
* INPUT:  PARAMETER 1 -  A(STATION TEXT)         - A(CL5)                       
*         PARAMETER 2 -  BYTE1 = 0               - INVENTORTY # FETCH           
*                         BYTES2-4 A(INV#)       - A(CL4)                       
*                     -  BYTE1 = X'80'           - DAY/TIME FETCH               
*                         BYTES2-4 A(CLUSTER)    - A(DETAIL CLUSTER)            
*                                                                               
*         PARAMETER 3 -  A(EFF DATES)            - A(2CL3) PWOS JULIAN          
*         PARAMETER 4 -  A(BOOKLIN)              - SEE BOOKLIN REPRO23          
*                                                                               
*         PARAMETER 5 -  A(DEMO)                 - A(XL3)                       
*                                                                               
* OUTPUT  PARAMETER 1 - CONTAINS DEMO VALUE                                     
*         PARAMETER 2 - CONTAINS SHARE VALUE                                    
*         PARAMETER 3 - CONTAINS LEVEL VALUE                                    
*                                                                               
* NOTES                                                                         
*         THIS ROUTINE USES AIOB(AIOREC) TO BUILD THE FETCH PARAMETER           
*         PARAMETER AS WELL AS PASSING AIO1-4 & AIO6 TO FETCH                   
*                                                                               
***********************************************************************         
AFETCH   DS    0H                                                               
*                                                                               
         BAS   RE,SAVVAL0                                                       
*                                                                               
         L     R4,AIOREC           CLEAR THE BLOCK                              
         LH    R5,=Y(IOAREALN)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R4,RE                                                            
         L     R4,AIOREC           CLEAR THE BLOCK                              
*                                                                               
         LA    R3,RFTBLKL(R4)      BUILD UPGRADES AFTER BLOCK                   
*                                                                               
         USING RFTBLKD,R4                                                       
         MVC   RFTACOM,ACOM                    A(COMFACS)                       
         MVC   RFTAIO1,AIO1                    A(2K IO AREA)                    
         MVC   RFTAIO2,AIO6                    A(2K IO AREA)                    
         MVC   RFTAWRK,AIO2                    A(6K WORK AREA)                  
*                                              USES AIO2,AIO3, & AIO4           
         LA    RE,FTCHHOOK                                                      
         STCM  RE,15,RFTHOOKA                           HOOK ROUTINE            
         MVI   RFTCNTL,RFTCHDRQ+RFTCDEMQ+RFTCSLVQ       DATA FLAGS              
         MVC   RFTCREP,CUAALF                           REP CODE                
         L     RE,RTPARMS1                                                      
         MVC   RFTCSTAT,0(RE)                 STATION CALL LETTERS              
         CLI   RFTCSTAT+4,C' '                NEED 'T' SET?                     
         BNE   *+8                            NO                                
         MVI   RFTCSTAT+4,C'T'                                                  
         MVI   RFTCSRC,C'N'                   DEMO SOURCE                       
*                                                                               
         TM    RTPARMS2,X'80'                 INVENTORY # FETCH?                
         BO    FTCH10                         NO                                
*                                                                               
         L     RE,RTPARMS2                                                      
         MVI   RFTAMODE,RFTAMSTQ                        FETCH MODE              
         MVC   RFTCINV,0(RE)                            INVENTORY #             
         L     R2,RTPARMS3                                                      
         OC    0(3,R2),0(R2)                                                    
         BZ    FTCH2                                                            
         GOTO1 VDATCON,RTPARM,(8,0(R2)),(2,RFTCEFST)     EFF START &            
FTCH2    OC    3(3,R2),3(R2)                                                    
         BZ    FTCH4                                                            
         GOTO1 VDATCON,RTPARM,(8,3(R2)),(2,RFTCEFEN)     END DATES              
FTCH4    DS    0H                                                               
         B     FTCH20                                                           
*                                                                               
FTCH10   MVI   RFTAMODE,RFTADIRQ                                                
         L     R6,RTPARMS2                                                      
         USING RPRDTELD,R6                                                      
         MVC   RFTCDTMS+1(1),RPRDTDAY                   PRIME DAYS              
         MVC   RFTCDTMS+2(4),RPRDTSTM                   PRIME TIMES             
         DROP  R6                                                               
*                                                                               
FTCH12   ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         CLI   0(R6),0                                                          
         BE    FTCH16                                                           
         CLI   0(R6),RPRDYELQ      SECONDARY DAYS/TIMES                         
         BE    *+8                                                              
         B     FTCH12                                                           
         ZIC   RE,1(R6)                                                         
         LA    RE,0(RE,R6)         END OF ELEM                                  
         LA    R6,RPRDYDTM-RPRDYELD(R6)                                         
         LA    RF,RFTCDTMS+RFTCDTLQ                                             
FTCH14   CR    R6,RE                                                            
         BNL   FTCH16                                                           
         MVC   1(L'RPRDYDTM,RF),0(R6)                                           
         LA    R6,L'RPRDYDTM(R6)                                                
         LA    RF,RFTCDTLQ(RF)                                                  
         LA    R0,RFTCDTMS+(8*RFTCDTLQ)                                         
         CR    RF,R0                                                            
         BL    FTCH14                                                           
*                                                                               
FTCH16   DS    0H                                                               
*                                                                               
FTCH20   L     R6,RTPARMS4                                                      
         USING BOOKLIN,R6                                                       
         XC    RFTCUPGA,RFTCUPGA                                                
         OC    BKLNUPGD,BKLNUPGD              UPGRADE EXPRESSION?               
         BNZ   FTCH22                         YES                               
*                                                                               
         MVC   RFTCBKS(L'BKLNBK),BKLNBK                                         
         MVC   RFTCBKS+L'BKLNBK+L'BKLNFIL(L'BKLNSPBK),BKLNSPBK                  
         MVC   RFTCBKS+L'BKLNBK(L'BKLNFIL),BKLNFIL                              
*                                                                               
         CLI   RFTAMODE,RFTAMSTQ   INVENOTRY FETCH?                             
         BE    FTCH30              YES                                          
*                                                                               
         CLI   BKLNFIL,RPRBKINQ    DAYTIME INVENTORY SOURCE?                    
         BNE   FTCH30              NO                                           
*                                                                               
         TM    BKLNBK,RPRBKSES+RPRBKSPJ+RPRBKST2+RPRBKSTP                       
         BZ    *+14                 SKIP E/P/T/S BOOKS                          
         XC    0(L'RFTCBKS,R3),0(R3)                                            
         B     FTCHX                                                            
*                                                                               
         MVI   RFTCBKS+L'BKLNBK,RPRBKTPQ                                        
         B     FTCH30                                                           
*                                                                               
FTCH22   DS    0H                                                               
         CLI   RFTAMODE,RFTAMSTQ   INVENOTRY FETCH?                             
         BE    *+12                YES                                          
         CLI   BKLNFIL,RPRBKINQ    DAYTIME INVENTORY SOURCE?                    
         BE    FTCH30              YES                                          
*                                                                               
         LR    R0,R3                                                            
         MVC   0(L'BKLNBK,R3),BKLNBK                                            
         MVC   L'BKLNBK(L'BKLNFIL,R3),BKLNFIL                                   
         MVC   L'BKLNBK+L'BKLNFIL(L'BKLNSPBK,R3),BKLNSPBK                       
         MVC   L'BKLNBK+L'BKLNFIL+L'BKLNSPBK(L'BKLNXBKS,R3),BKLNXBKS            
         LA    R3,L'BKLNBK+L'BKLNFIL+L'BKLNSPBK+L'BKLNXBKS(R3)                  
*                                                                               
         MVI   0(R3),X'05'                                                      
         MVI   1(R3),14                                                         
         MVC   2(L'BKLNUPGD,R3),BKLNUPGD                                        
         STCM  R0,15,RFTCUPGA                                                   
*                                                                               
FTCH30   L     RE,RTPARMS5                    DEMO                              
         MVC   RFTCDEMS,0(RE)                                                   
         MVI   RFTCDEMS,0          ZAP FIRST BYTE FOR BRIAN                     
         DROP  R4                                                               
*                                                                               
         GOTO1 VFETCH,RTPARM,(R4)                                               
*                                                                               
FTCHX    MVC   RTPARMS1,RTDEMVAL              PASS THESE BACK                   
         MVC   RTPARMS2,RTSHRVAL                                                
         MVC   RTPARMS3,RTLVLVAL                                                
*                                                                               
         BAS   RE,RESVAL0                                                       
*                                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FETCH HOOK ROUTINE                                                            
***********************************************************************         
FTCHHOOK NTR1                                                                   
         L     R4,AIOREC                                                        
         USING RFTBLKD,R4                                                       
         OC    RFTERR,RFTERR                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    RFTMODE,RFTNBKQ     NEW BOOK DATA?                               
         BNO   FHOOKX              NO                                           
         SPACE 2                                                                
**************************************                                          
* SET THE VALUES FOR PASSING BACK                                               
**************************************                                          
         MVC   RTDEMVAL,RFTFDEMS                                                
         MVC   RTSHRVAL,RFTFSHRS                                                
         MVC   RTLVLVAL,RFTFLVLS                                                
         SPACE 1                                                                
FHOOKX   B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SAVE VALUES INTO TIA - DON'T USE GEIFILS SAVVAL                               
***********************************************************************         
SAVVAL0  NTR1                                                                   
SAVVAL1  DS    0H                                                               
         LH    RE,=Y(TWVALS-TWAD)                                               
         A     RE,ATWA             SAVE GLOBAL W/S VALUES IN TWA0               
         LA    R0,BCVALS                                                        
         LA    RF,BCVALSL                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,ATWA             SAVE CONTROLLER W/S VALUES IN TWA0           
         AH    RE,=Y(GSVALS-TWAD)                                               
         LA    R0,GSSAVE                                                        
         LA    RF,GSSAVEL                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         ICM   R3,12,=C'L='        SAVE TEMPSTR PAGE FOR FDRELDS                
         ICM   R3,3,=Y(TWAMAX-FDRDISPQ)                                         
         GOTO1 VDMGR,RTPARM,=CL8'DMREAD',=CL8'TEMPSTR',(4,0),ATIA,,(R3)         
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   R3,3,=Y(TWAMAX)                                                  
         GOTO1 (RF),(R1),=CL8'DMWRT',=CL8'TEMPSTR',(4,0),ATIA,,(R3)             
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* RESTORE VALUES INTO TIA - DON'T USE GEIFILS RESVAL                            
***********************************************************************         
RESVAL0  NTR1                                                                   
RESVAL1  DS    0H                                                               
         ICM   R3,12,=C'L='        RESTORE TEMPSTR PAGE FOR FDRELDS             
         ICM   R3,3,=Y(TWAMAX)                                                  
         GOTO1 VDMGR,RTPARM,=CL8'DMREAD',=CL8'TEMPSTR',(4,0),ATIA,,(R3)         
         BE    *+6                                                              
         DC    H'0'                                                             
RESVAL04 XR    R0,R0                                                            
         ICM   R0,1,GS#FDR                                                      
         BZ    RESVALX                                                          
         L     R1,ATIA             RESTORE FDRBLK                               
         AH    R1,=Y(FDRDISPQ)                                                  
         USING FDRELD,R1                                                        
         L     RE,AFDRADDR                                                      
         XR    RF,RF                                                            
RESVAL06 IC    RF,FDRLN                                                         
         CLI   FDREL,FDRELQ                                                     
         BE    *+12                                                             
         CLI   FDREL,FLTRLQ                                                     
         BNE   *+12                                                             
         ST    R1,0(RE)                                                         
         LA    RE,L'FDRADDR(RE)                                                 
         AR    R1,RF                                                            
         BCT   R0,RESVAL06                                                      
         DROP  R1                                                               
RESVALX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FETCH PRIME BOOK(ALL DEMOS) FOR THE PROPOSAL                                  
*                                                                               
* INPUT:  PARAMETER 1 -  A(SAVSTAS)              - FOR PROPOSAL                 
*         PARAMETER 2 -  A(BOOKLIN)              - FOR PRIME BOOK               
*         PARAMETER 3 -  A(SAVDMOS)              - FOR PROPOSAL                 
*                                                                               
* OUTPUT:                                                                       
*                                                                               
* NOTES                                                                         
*         THIS ROUTINE USES AIOB(AIOREC) TO BUILD THE FETCH PARAMETER           
*         AIO5 IS USED FOR RECUP                                                
*         MINIO SHOULD BE INITIALIZED                                           
*         PARAMETER AS WELL AS PASSING AIO1-4 & AIO6 TO FETCH                   
*                                                                               
***********************************************************************         
PRBKFTCH DS    0H                                                               
         BAS   RE,SAVVAL0                                                       
*                                                                               
         GOTO1 VCOLY,BODMCB,(X'25',0),(0,0)                                     
         CLI   BODMCB+4,X'FF'                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,BODMCB                                                        
         GOTO1 (RF),BODMCB,(R9),RTPARMS1,RTPARMS2,RTPARMS3                      
*                                                                               
         BAS   RE,RESVAL0                                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FETCH DETAIL LINE FETCH ALL BOOKS & DEMOS                                     
*                                                                               
* INPUT:  PARAMETER 1 -  A(SAVSTAS)              - FOR PROPOSAL                 
*         PARAMETER 2 -  A(BOOKLIN)              - FOR PRIME BOOK               
*         PARAMETER 3 -  A(SAVDMOS)              - FOR PROPOSAL                 
*         PARAMETER 4 -  A(MINIOKEY)             - SANS ELCODE & LEN            
*                                                                               
* OUTPUT:                                                                       
*                                                                               
* NOTES                                                                         
*         THIS ROUTINE USES AIOB(AIOREC) TO BUILD THE FETCH PARAMETER           
*         AIO5 IS USED FOR RECUP                                                
*         MINIO SHOULD BE INITIALIZED                                           
*         PARAMETER AS WELL AS PASSING AIO1-4 & AIO6 TO FETCH                   
*                                                                               
***********************************************************************         
DTLNFT   NTR1                                                                   
DTLNFTCH DS    0H                                                               
         CLI   RTPARMS4,C'R'       REPORT                                       
         BE    *+8                 YES  - TIA FOR PRINTQ                        
         BAS   RE,SAVVAL0                                                       
*                                                                               
         GOTO1 VCOLY,BODMCB,(X'26',0),(0,0)                                     
         CLI   BODMCB+4,X'FF'                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,BODMCB                                                        
         GOTO1 (RF),BODMCB,(R9),RTPARMS1,RTPARMS2,RTPARMS3,RTPARMS4             
*                                                                               
         CLI   RTPARMS4,C'R'       REPORT                                       
         BE    *+8                 YES  - TIA FOR PRINTQ                        
         BAS   RE,RESVAL0                                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FETCH DETAIL LINE REFETCH ALL BOOKS & DEMOS                                   
*                                                                               
* INPUT:  PARAMETER 1 -  A(SAVSTAS)              - FOR PROPOSAL                 
*         PARAMETER 2 -  A(BOOKLIN)              - FOR PRIME BOOK               
*         PARAMETER 3 -  A(SAVDMOS)              - FOR PROPOSAL                 
*         PARAMETER 4 -  A(MINIOKEY)             - SANS ELCODE & LEN            
*                                                                               
* OUTPUT:                                                                       
*                                                                               
* NOTES                                                                         
*         MINIO SHOULD BE INITIALIZED                                           
*         THIS ROUTINE CALLS DTLNFTCH                                           
*                                                                               
***********************************************************************         
DTLNREFT DS    0H                                                               
*-------------------------------------------------------------*                 
*            RETRIEVE CLUSTERS AND DELETE DEMO VALUE ELEMENTS *                 
*-------------------------------------------------------------*                 
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     RF,RTPARMS4                                                      
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDTELQ    DETAIL CLUSTER                               
         MVC   MINEKEY+1(7),0(RF)                                               
         GOTO1 VMINIO,BODMCB,('MINHI',(R5))                                     
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    *+6                 WHO PASSED A BOOGUS KEY?                     
         DC    H'0'                                                             
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         CLI   0(R6),RPRDTELQ                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   MINEKEY+1(7),2(R6)                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
         SR    R2,R2               MOVE TO                                      
         SR    RE,RE               MOVE FROM                                    
         SR    RF,RF               MOVE FROM                                    
*                                                                               
DTLNR2   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             END OF CLUSTER?                              
         BE    DTLNR6              YES                                          
         CLI   0(R6),RPRDVELQ      DEMO VALUE ELEMENT                           
         BH    DTLNR4              NO - PAST THEM                               
         BL    DTLNR2                                                           
*                                                                               
         LTR   R2,R2                                                            
         BNZ   *+8                                                              
         LA    R2,0(R6)                                                         
         B     DTLNR2                                                           
*                                                                               
DTLNR4   DS    0H                  DETERMINE FROM LENGTH                        
         LA    RE,0(R6)                                                         
         L     R0,MINELEM                                                       
         SR    R6,R0                                                            
         LH    RF,MINELEML                                                      
         SR    RF,R6               LENGTH OF FROM FIELD                         
*                                                                               
DTLNR6   DS    0H                  DETERMINE TO LENGTH                          
         LTR   R2,R2                                                            
         BZ    DTLNR8                                                           
         L     R0,MINELEM                                                       
         LR    R1,R2                                                            
         SR    R1,R0                                                            
         LH    R3,MINELEML                                                      
         SR    R3,R1               LEGTH OF TO FIELD                            
*                                                                               
         MVCL  R2,RE                                                            
         L     RF,MINELEM                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(1),0(RF)                                                 
         MVC   MINEKEY+1(L'RPROKMEL-1),2(RF)                                    
*                                                                               
         GOTO1 VMINIO,BODMCB,('MINWRT',(R5))                                    
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
*                                                                               
DTLNR8   OI    RTPARMS2,X'80'                                                   
         BAS   RE,DTLNFT           CALL DTLNFTCH(W/NTR1)                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FETCH DETAIL LINE BOOKS DEMO & FOOTNOTES                                      
*                                                                               
***********************************************************************         
DTLNDMFN DS    0H                                                               
         BAS   RE,SAVVAL0                                                       
*                                                                               
         GOTO1 VCOLY,BODMCB,(X'27',0),(0,0)                                     
         CLI   BODMCB+4,X'FF'                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,BODMCB                                                        
         GOTO1 (RF),BODMCB,(R9),RTPARMS1,RTPARMS2,RTPARMS3,RTPARMS4             
*                                                                               
         BAS   RE,RESVAL0                                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* GET PARENT REP                                                                
*                                                                               
*  INPUT:  PARAMETER 1     BYTES 1+2       REPCODE                              
*                                                                               
*  OUPUT:  PARAMETER 1     BYTES 1+2       PARENT REP CODE                      
*                                                                               
*  NOTE THIS ROUTINE SMASHES AIO4                                               
***********************************************************************         
GETPARNT DS    0H                                                               
         XC    IOKEY,IOKEY                                                      
K        USING RREPKEY,IOKEY                                                    
         MVI   K.RREPKTYP,X'01'                                                 
         MVC   K.RREPKREP,RTPARMS1                                              
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX (XIO,AGROUTS)                                                    
         BE    *+6                                                              
         DC    H'0'                SCREW UP ON THE READ HIGH                    
         DROP  K                                                                
*                                                                               
         CLC   IOKEY(L'RREPKEY),IOKEYSAV                                        
         BE    *+6                                                              
         DC    H'0'                NO REP RECORD                                
*                                                                               
         L     R1,=AL4(XOREPFIL+XOGET+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO4                                                          
         LA    R2,RREPELEM-RREPREC(R2)                                          
         CLI   0(R2),X'01'                                                      
         BE    *+6                                                              
         DC    H'0'                LOST REP ELEMENT                             
         USING RREPELEM,R2                                                      
         MVC   RTPARMS(2),RREPPAR                                               
         DROP  R2                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* INITIALIZE MINIO                                                              
***********************************************************************         
MNIOINIT DS    0H                                                               
         L     R0,AIO7             IO7 IS USED FOR MINBLK & MINELEM             
         LA    R1,IOAREALN                                                      
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR THEM BOTH                              
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     RF,=V(RECUP)                                                     
         A     RF,RTRELO                                                        
         ST    RF,MINRECUP                                                      
         MVC   MINCOMF,ACOM                                                     
         MVI   MINOPEN,C'N'        SET NOT OPEN                                 
         MVC   MINFIL,REPFIL       FILE NAME                                    
         MVC   MINDIR,REPDIR       DIRECTORY NAME                               
         MVI   MINFKLEN,L'RPROKEY    KEY LENGTH                                 
         MVI   MINEKLEN,L'RPROKMEL   ELEMENT KEY LENGTH                         
         MVI   MINEKDSP,L'RPROKMST   DISPLACEMENT TO ELEMENT KEY                
         MVC   MINAGYC,CUAALF             AGENCY                                
         MVI   MINAGYD,RPROKRCD-RPROKEY   DISP TO POWER CODE IN KEY             
         MVI   MINNCTL,L'RPROKCTL    NUMBER OF CONTROL BYTES                    
         MVC   MINFRCLM,=AL2(1925)   MAXIMUM RECORD LENGTH                      
         MVC   MINBUFF,AIO8        A(FIRST BUFFER)                              
         MVI   MINNBUF,2           NUMBER OF BUFFERS AVAILABLE                  
*        LR    R1,R9                                                            
*        A     R1,=AL4(MINSTRT-WORKD)                                           
*        ST    R1,MINRTAB             A(MINIO RECORD TABLE)                     
         MVC   MINRTAB,AREPRO02    <=== SET IN REPROINI                         
         MVC   MINRTABL,=Y(LENMINIO)  L(MINIO RECORD TABLE)                     
*                                                                               
         MVI   MINNKLO,RPRDYELQ    ELEMENTS IN CLUSTER THAT DON'T NEED          
         MVI   MINNKHI,RPRNDELQ       A MINIO ELEM KEY (X'61'-X'8F')            
*                                                                               
         L     R1,AIO7             USE 2ND HALF OF IO7 FOR MINIO ELEM           
         LA    R1,MINBLKL(R1)                                                   
         ST    R1,MINELEM          A(AREA FOR ELEM OR CLUSTER)                  
         MVC   MINMAXEL,=Y(IOAREALN-MINBLKL)  MAXLEN(ELEM OR CLUSTER)           
*                                                                               
         XC    MINMKEY,MINMKEY     SET MASTER KEY FOR MINIO                     
         LA    R4,MINMKEY                                                       
         MVC   0(L'RPROKMST,R4),GSRECKEY                                        
*                                                                               
MNOINIX  B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PACK THE START AND END TIMES  (COPIED FROM SPNWS00)                           
*                                                                               
* ON ENTRY:    PARAM 1             A(START TIME MILITARY) (0-2359)              
*              PARAM 2             A(END   TIME MILITARY) (0-2359)              
*              PARAM 3             A(PACKED TIME)                               
*                                      START TIME SHIFTED LEFT 4 BITS           
*                                        LAST 4 BITS = # QTR HOURS              
***********************************************************************         
PACKTIME DS    0H                                                               
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
         CLC   0(L'RPRDPSTM,R2),0(R3)                                           
         BNH   *+16                                                             
         LH    R1,0(R3)                                                         
         AH    R1,=H'2400'                                                      
         STH   R1,0(R3)                                                         
*                                                                               
         SR    R0,R0               CALCULATE # OF QTR HRS                       
         LH    R1,0(R2)            0 = 00-15 MINS                               
         SR    RE,RE               1 = 16-30 MINS                               
         SR    RF,RF               2 = 31-45 MINS, ETC..                        
         ICM   RF,3,0(R3)                                                       
         BZ    PTIM2                                                            
*                                                                               
         D     R0,=F'100'          SEPARATE HOURS AND MINUTES                   
         D     RE,=F'100'                                                       
         SR    RF,R1                                                            
         MH    RF,=H'60'                                                        
         AR    RF,RE                                                            
         SR    RF,R0                                                            
         SR    RE,RE                                                            
         D     RE,=F'15'                                                        
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         LTR   RE,RE                                                            
         BNZ   *+6                                                              
         BCTR  RF,0                                                             
*                                                                               
         C     RF,=F'15'                                                        
         BNH   PTIM2                                                            
         L     RF,=F'15'                                                        
*                                                                               
PTIM2    SR    RE,RE               PACKED TIMES                                 
         ICM   RE,3,0(R2)                                                       
         SLL   RE,4                                                             
         AR    RE,RF                                                            
         STCM  RE,3,0(R4)          STORE IN IN HIGH ORDER NIBBLE                
*                                                                               
PTIMEX   B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* GETPROF - GET THE PROGRAM PROFILES                                            
*   INPUT:   P1  BYTE 1      PROGRAM #                                          
*                BYTE 2-4    A(PROFILE AREA) CL10                               
*                                                                               
*   OUPUT:   PROFILES IN PROFILE AREA                                           
*                                                                               
***********************************************************************         
GETPROF  DS    0H                                                               
         L     R2,RTPARMS1                                                      
         XC    0(10,R2),0(R2)                                                   
*                                                                               
         XC    IOKEY,IOKEY         GET REP RECORD                               
         MVI   IOKEY,X'01'                                                      
         MVC   IOKEY+25(2),CUAALF                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOREAD)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   GETPROFX                                                         
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPFIL+XOGET)                                  
         GOTOX ('XIO',AGROUTS)                                                  
         BNL   *+6                                                              
         DC    H'0'                                                             
         BH    GETPROFX                                                         
*                                                                               
         L     R6,AIO4                                                          
         MVI   RTELCODE,X'04'                                                   
         LA    R6,RREPELEM-RREPREC(R6)                                          
         BAS   RE,FIRSTEL                                                       
         BNE   GETPROFX            NO PROFILE ELEMENT                           
         USING RREPPGMP,R6                                                      
         ZIC   RF,RREPPGM#         # OF PROGRAM UNITS (LOOP COUNTER)            
         LA    R6,RREPPGM1                                                      
         USING RREPPGM1,R6                                                      
GPROF10  CLC   RREPPGM1(1),RTPARMS1   CORRECT PROGRAM?                          
         BE    GPROF20                                                          
         LA    R6,RREPPGML(R6)                                                  
         BCT   RF,GPROF10                                                       
         B     GETPROFX            CONTRACT NOT FOUND. USE DEFAULTS.            
*                                                                               
GPROF20  MVC   0(10,R2),RREPPGM1   SAVE PROGRAM PROFILES UNIT                   
         DROP  R6                                                               
GETPROFX B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         GETEL R6,RCONELEM-RCONKEY,RTELCODE                                     
*                                                                               
SPACES   DC    80C' '                                                           
REPDIR   DC    CL8'REPDIR'                                                      
REPFIL   DC    CL8'REPFIL'                                                      
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
RTWORKD  DSECT                                                                  
RTRELO   DS    A                                                                
RTPARMA  DS    A                   A(INCOMING PARAMETER LIST)                   
RTPARMO  DS    F                                                                
RTPARMS  DS    0XL24               SAVED PARAMETERS                             
RTPARMS1 DS    A                                                                
RTPARMS2 DS    A                                                                
RTPARMS3 DS    A                                                                
RTPARMS4 DS    A                                                                
RTPARMS5 DS    A                                                                
RTPARMS6 DS    A                                                                
RTPARM   DS    XL24                * PARAMETERS 1-6 *                           
RTDEMVAL DS    F                   DEMO VALUE VIA FETCH                         
RTSHRVAL DS    F                   SHARE VALUE VIA FETCH                        
RTLVLVAL DS    F                   LEVEL VALUE VIA FETCH                        
RTELCODE DS    X                                                                
RTBYTE2  DS    X                                                                
RTBYTE3  DS    X                                                                
RTBYTE4  DS    X                                                                
*                                                                               
RTWORKL  EQU   *-RTWORKD                                                        
         EJECT                                                                  
       ++INCLUDE REPROLN                                                        
         EJECT                                                                  
* REPROWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE REPROWORK                                                      
         PRINT ON                                                               
* REFETCHD                                                                      
         PRINT OFF                                                              
       ++INCLUDE REFETCHD                                                       
         PRINT ON                                                               
PRO01    CSECT                                                                  
         ORG   PRO01+(((*-PRO01)/2048)+1)*2048                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018REPRO01S  05/01/02'                                      
         END                                                                    
