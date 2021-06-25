*          DATA SET SRVER00    AT LEVEL 006 AS OF 06/22/12                      
*PHASE T14500A                                                                  
         TITLE '$VERSION - DISPLAY CHANGE VERSION TABLE'                        
         PRINT NOGEN                                                            
VERSION  CSECT                                                                  
         NMOD1 WORKL,**$VER**,RA,CLEAR=YES,RR=RE                                
         USING WORKD,RC                                                         
         USING DMSPACED,DSPHD                                                   
         ST    RD,BASERD                                                        
         ST    RE,RELO                                                          
         MVC   IPARMS,0(R1)                                                     
*                                                                               
         L     R9,ASYSFACS                                                      
         USING SYSFACD,R9          R9=A(SYS FAC LIST)                           
         L     R8,ATWA                                                          
         USING SRVERFFD,R8         R8=A(TWA)                                    
         MVC   ASAVE,ATIA          USE TIA AS SAVE AREA                         
         BRAS  RE,INIT                                                          
         BRAS  RE,SETFFFF          MAKE SURE LAST UPDATE TIME IS THERE          
*                                                                               
         BRAS  RE,P1VAL            VALIDATE PARAMETERS                          
         BNE   MAIN06                                                           
         BRAS  RE,P2VAL                                                         
         BNE   MAIN06                                                           
         BRAS  RE,P3VAL                                                         
         BNE   MAIN06                                                           
*                                                                               
         BRAS  RE,SCROLL                                                        
         TM    DDS,DDSNEW          NEW CONNECT - DISPLAY THEN LEAVE             
         BZ    *+12                                                             
         BRAS  RE,DISPLAY                                                       
         B     MAIN08                                                           
*                                                                               
         OC    SVTIME,SVTIME       GOOD DISPLAY BEFORE?                         
         BNZ   *+12                YES - CAN VALIDATE                           
         BRAS  RE,DISPLAY                                                       
         B     MAIN08                                                           
*                                                                               
         BRAS  RE,NEEDVAL          INPUT TO A SELECT FIELD?                     
         BNE   *+12                YES                                          
         BRAS  RE,DISPLAY                                                       
         B     MAIN08                                                           
*                                                                               
         BRAS  RE,CHKTIME          DISPLAY AFTER TIME OF LAST CHANGE?           
         BNL   MAIN04              YES                                          
         MVC   FERN,=AL2(FVFNUP)                                                
         B     MAIN06                                                           
*                                                                               
*              MAKE SURE FILTERS DIDN'T CHANGE                                  
*                                                                               
MAIN04   BRAS  RE,VALIDATE         VALIDATE ANY CHANGES                         
         BNE   MAIN06                                                           
         BRAS  RE,SQUASH           CLOSE ANY GAPS                               
*        BRAS  RE,SORT             SORT PHASES                                  
         BRAS  RE,CLRSCRN                                                       
         BRAS  RE,DISPLAY          REDISPLAY SCREEN                             
*                                                                               
MAIN06   TM    DDS,DDSUPD          TEST FOR UPDATE                              
         BZ    MAIN08                                                           
         L     R1,VSSB             SET CHECKPOINT FLAG                          
         OI    SSBSTAT1-SSBD(R1),SSBSCHK1                                       
         MVC   HDRN,=AL2(INFUPD)                                                
*                                                                               
MAIN08   XR    RF,RF               DISPLAY PAGE+1                               
         IC    RF,PAGE                                                          
         LA    RF,1(RF)                                                         
         EDIT  (RF),(3,SRVP1),ALIGN=LEFT                                        
*                                                                               
         OC    FERN,FERN                                                        
         BZ    MAIN10                                                           
         BRAS  RE,DISERR                                                        
         BRAS  RE,WRITESTR                                                      
         B     XMOD                                                             
*                                                                               
MAIN10   BRAS  RE,DISOK                                                         
         BRAS  RE,WRITESTR                                                      
         B     XMOD                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY FROM DATASPACE                                              *         
***********************************************************************         
         SPACE 1                                                                
DISPLAY  NTR1  ,                                                                
         BRAS  RE,CLRSCRN          CLEAR SCREEN AND                             
         LA    RF,SRVP1H           SET DEFAULT CURSOR POSITION                  
         ST    RF,FADRH                                                         
*                                                                               
         NI    DDS,255-DDSEOT                                                   
*                                                                               
         XR    R2,R2               MAKE SURE PAGE INDEX MAKES SENSE             
         IC    R2,PAGE                                                          
         MHI   R2,PERPAGE          NUMBER OF ENTRIES ON A PAGE                  
         MH    R2,DSPTWIDE                                                      
         AH    R2,DSPTWIDE         ADD 1 FOR FFFFFF ENTRY                       
*                                                                               
         A     R2,DSPTFRST         R2=A(FIRST ENTRY TO DISPLAY)                 
         C     R2,DSPTEND                                                       
         BL    *+14                INVALID PAGE NUMBER                          
         MVC   FERN,=AL2(FVFBIG)                                                
         B     EXITL                                                            
*                                                                               
         BRAS  RE,LOCKSPC          MAKE DFS NO-ONE ELSE IS DOING THIS           
         LH    R4,DSPTWIDE         SET FOR BXLE ON R2,R4,R5                     
         L     R5,DSPTEND                                                       
*                                                                               
         LA    R6,SRVSELH          R6=START OF SCREEN                           
DIS02    BRAS  RE,ARSOFF           MAKE SURE ACCESS REGISTERS OK                
         LAM   AR2,AR2,ALET                                                     
         SAC   512                                                              
         USING VRSNTABD,R2                                                      
         OC    VRSNPHS,VRSNPHS                                                  
         BZ    DIS04                                                            
         MVC   VIRGIN,0(R2)        GET A LOCAL COPY OF VERSION TABLE            
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,FILTERS                                                       
         BNE   DIS04                                                            
         BRAS  RE,VIRDISP          DISPLAY ENTRY                                
*                                                                               
         LA    R0,SRVSELRH         FINISHED SCREEN?                             
         CR    R6,R0                                                            
         BE    DISX                YES                                          
*                                                                               
         LA    R0,SRVSELLH         TEST END OF LHS                              
         CR    R6,R0                                                            
         BNE   *+12                                                             
         LA    R6,SRVSEL2H         START RHS                                    
         B     *+8                                                              
         AHI   R6,(VLLINEL)*2      NEXT LINE ON SCREEN                          
*                                                                               
DIS04    BXLE  R2,R4,DIS02                                                      
         OI    DDS,DDSEOT          END OF TABLE                                 
*                                                                               
DISX     BRAS  RE,ARSOFF                                                        
         BRAS  RE,ULOCKSPC         UNLOCK DATASPACE                             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MAIN VALIDATION LOOP                                                *         
***********************************************************************         
         SPACE 1                                                                
VALIDATE NTR1  ,                                                                
         XR    R1,R1               MAKE SURE PAGE INDEX MAKES SENSE             
         IC    R1,PAGE                                                          
         MHI   R1,PERPAGE          NUMBER OF ENTRIES ON A PAGE                  
         MH    R1,DSPTWIDE                                                      
*                                                                               
         LH    R4,DSPTWIDE         SET FOR BXLE ON R2,R4,R5                     
         ICM   R5,15,DSPTEND                                                    
*                                                                               
         ICM   R2,15,DSPTFRST                                                   
         AR    R2,R1               R2=A(FIRST ENTRY TO DISPLAY)                 
         AH    R2,DSPTWIDE         GO PAST FFFFFF ENTRY FOR TIME                
         C     R2,DSPTEND                                                       
         BNL   EXITNV              INVALID PAGE NUMBER                          
*                                                                               
         BRAS  RE,LOCKSPC          LOCK DATASPACE                               
*                                                                               
         LA    R6,SRVSELH          R6=START OF SCREEN                           
         USING VERLINED,R6                                                      
VAL02    LA    R1,VLSELH           TEST INPUT TO SELECT FIELD                   
         USING FHD,R1                                                           
         ST    R1,FADRH                                                         
         CLI   FHIL,0                                                           
         BE    VAL14                                                            
         CLI   FHDA,C'*'           ALREADY PROCESSED                            
         BNE   *+12                                                             
         MVI   FHDA,C' '           SO CLEAR IT                                  
         B     VAL14                                                            
*                                                                               
         MVC   ACTN,FHDA                                                        
         CLI   FHDA,C'D'                                                        
         BE    VAL07                                                            
         CLI   FHDA,C'C'                                                        
         BE    VAL07                                                            
         CLI   FHDA,C'A'                                                        
         BE    VAL08                                                            
         MVC   FERN,=AL2(FVFACT)                                                
         B     VALBAD                                                           
*                                                                               
VAL07    CLI   FLSYS,0                                                          
         BE    VAL08                                                            
         MVC   FERN,=AL2(FVFBACT)                                               
         B     VALBAD                                                           
*                                                                               
VAL08    MVI   FHDA,C'*'           SET LINE PROCESSED                           
         DROP  R1                                                               
*                                                                               
         BRAS  RE,VIRVAL           VALIDATE INPUT LINE                          
         BNE   VALBAD                                                           
*                                                                               
         OI    DDS,DDSUPD          SET UPDATE OCCURRED                          
         LAM   AR0,ARF,ARZERO      MAKE SURE ACCESS REGISTERS OK                
         LAM   AR2,AR2,ALET                                                     
         SAC   512                                                              
*                                                                               
         CLI   ACTN,C'A'           TEST ADD ENTRY                               
         BE    *+14                                                             
         MVC   VRSNTABD(VRSNLEN),VIRGIN  UPDATE THIS ENTRY                      
         B     VAL12                                                            
*                                                                               
         CPYA  AR3,AR2             FIND EMPTY TABLE ENTRY                       
         ICM   R3,15,DSPTFRST                                                   
         OC    0(VRSNLEN,R3),0(R3)                                              
         BZ    VAL10                                                            
         BXLE  R3,R4,*-10                                                       
         MVC   FERN,=AL2(FVFFULL)                                               
         B     VALBAD                                                           
*                                                                               
VAL10    MVC   0(VRSNLEN,R3),VIRGIN      ADD NEW ENTRY                          
*                                                                               
VAL12    CPYA  AR3,AR2             UPDATE TIMESTAMP IN FIRST ENTRY              
         ICM   R3,15,DSPTFRST                                                   
         MVC   VRSNADV-VRSNTABD(4,R3),MYTIME                                    
*                                                                               
VAL14    BRAS  RE,ARSOFF           CLEAR OUT OF ACCESS REGISTERS                
         LA    R0,SRVSELRH         FINISHED SCREEN?                             
         CR    R6,R0                                                            
         BE    VALX                NO                                           
*                                                                               
         LA    R0,SRVSELLH         END OF LHS?                                  
         CR    R6,R0                                                            
         BNE   *+12                                                             
         LA    R6,SRVSEL2H         START RHS                                    
         B     *+8                                                              
         AHI   R6,(VLLINEL)*2      NEXT LINE ON SCREEN                          
*                                                                               
         BXLE  R2,R4,VAL02         NEXT TABLE ENTRY                             
         OI    DDS,DDSEOT                                                       
*                                                                               
VALX     BRAS  RE,ARSOFF                                                        
         BRAS  RE,ULOCKSPC                                                      
         B     EXITOK                                                           
*                                                                               
VALBAD   BRAS  RE,ARSOFF                                                        
         BRAS  RE,ULOCKSPC                                                      
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DETERMINE WHETHER A SELECT FIELD HAS BEEN INPUT          *         
***********************************************************************         
         SPACE 1                                                                
NEEDVAL  NTR1  ,                                                                
         LA    R6,SRVSELH          R6=START OF SCREEN                           
         USING VERLINED,R6                                                      
NVAL02   LA    R1,VLSELH           TEST INPUT TO SELECT FIELD                   
         USING FHD,R1                                                           
         CLI   FHIL,0                                                           
         BE    NVAL04              FIELD INPUT                                  
*                                                                               
         CLI   FHDA,C'*'           ALREADY PROCESSED                            
         BNE   EXITL                                                            
         MVI   FHDA,C' '           SO CLEAR IT                                  
*                                                                               
NVAL04   LA    R0,SRVSELRH         FINISHED SCREEN?                             
         CR    R6,R0                                                            
         BE    EXITOK              YES                                          
*                                                                               
         LA    R0,SRVSELLH         END OF LHS?                                  
         CR    R6,R0                                                            
         BNE   *+12                                                             
         LA    R6,SRVSEL2H         START RHS                                    
         B     *+8                                                              
         AHI   R6,(VLLINEL)*2      NEXT LINE ON SCREEN                          
         B     NVAL02                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK TO SEE IF WE CAN DO VALIDATION - SVTIME > TIME IN DATASPACE   *         
***********************************************************************         
         SPACE 1                                                                
CHKTIME  NTR1  ,                                                                
         LAM   AR0,ARF,ARZERO      MAKE SURE ACCESS REGISTERS OK                
         LAM   AR2,AR2,ALET                                                     
         ICM   R2,15,DSPTFRST                                                   
         SAC   512                                                              
         CLC   VRSNPHS,EFFS        FIRST ENTRY SHOULD ALREADY BE FF'S           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   SVTIME,VRSNADV      COMPARE TIMES                                
         SAC   0                                                                
         LAM   AR2,AR2,ARZERO                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ENSURE WE HAVE A VALID TIMESTAMP                                    *         
* NTRY: EXPECTS DATASPACE TO BE LOCKED                                *         
* TIMESTAMP IS FIRST ENTRY - VRSNPHS IS X'FFFFFF'                     *         
*                          - VRSNADV(4) IS TIME IN TUS OF LAST UPDATE *         
***********************************************************************         
         SPACE 1                                                                
SETFFFF  NTR1  ,                                                                
         BRAS  RE,LOCKSPC                                                       
         LAM   AR0,ARF,ARZERO      MAKE SURE ACCESS REGISTERS OK                
         LAM   AR2,AR2,ALET                                                     
         ICM   R2,15,DSPTFRST                                                   
         LH    RE,DSPTWIDE         SET UP FOR BXLE BELOW                        
         ICM   RF,15,DSPTEND                                                    
         SAC   512                                                              
         USING VRSNTABD,R2                                                      
         CLC   VRSNPHS,EFFS        FIRST ENTRY SHOULD ALREADY BE FF'S           
         BE    SFF06                                                            
*                                                                               
* WE DON'T HAVE A VALID TIMESTAMP AS THE FIRST ENTRY                            
* SO WE WILL FIND THE FIRST EMPTY ENTRY AND MAKE A VALID TIMESTAMP              
* THEN SWAP THE VALID TIMESTAMP AND THE FIRST ENTRY WITH ONE ANOTHER            
*                                                                               
SFF02    OC    VRSNTABD(VRSNLEN),VRSNTABD                                       
         BZ    SFF04               FOUND AN EMPTY ENTRY                         
         BXLE  R2,RE,SFF02                                                      
         BRAS  RE,ULOCKSPC         ERROR - VERSION TABLE IS FULL                
         DC    H'0'                                                             
*                                                                               
SFF04    MVC   VRSNPHS,EFFS        SET TIME OF LAST UPDATE                      
         MVC   VRSNADV(4),MYTIME                                                
*                                                                               
         C     R2,DSPTFRST         TABLE WAS EMPTY?                             
         BE    SFF06               YES - NOW IT JUST HAS A TIMESTAMP            
*                                                                               
         CPYA  AR3,AR2                                                          
         ICM   R3,15,DSPTFRST      SWAP TIMESTAMP WITH FIRST ENTRY              
         XC    0(VRSNLEN,R2),0(R3)                                              
         XC    0(VRSNLEN,R3),0(R2)                                              
         XC    0(VRSNLEN,R2),0(R3)                                              
*                                                                               
         XC    SVTIME,SVTIME       MAKE SURE NEVER HAVE VALID TIME              
*                                                                               
SFF06    BRAS  RE,ARSOFF                                                        
         BRAS  RE,ULOCKSPC                                                      
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         TIME  TU                                                               
         ST    R0,MYTIME                                                        
*                                                                               
         L     RF,AUTL             EXTRACT UTL DATA                             
         USING UTLD,RF                                                          
         MVC   TRM,TNUM                                                         
         TM    TSVCREQ,X'01'       TEST FIRST TIME                              
         BNO   *+12                                                             
         OI    TSVCREQ,X'02'                                                    
         OI    DDS,DDSNEW                                                       
         DROP  RF                                                               
*                                                                               
         L     RF,ATIOB            EXTRACT TIOB DATA                            
         USING TIOBD,RF                                                         
         MVC   PFKEY,TIOBAID                                                    
*                                                                               
         SR    RE,RE               PF1 HELP                                     
         ICM   RE,3,TIOBCURD                                                    
         AR    RE,R8                                                            
         CLI   PFKEY,1             TEST HELP PFKEY                              
         BNE   INIT02                                                           
         ST    RE,AHELP            SAVE A(HELP FIELD)                           
         MVI   PFKEY,0                                                          
         DROP  RF                                                               
*                                                                               
INIT02   L     RF,ACOMFACS         EXTRACT COMFACS ADDRESSES                    
         USING COMFACSD,RF                                                      
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VGETHELP,CGETHELP                                                
         MVC   VSCANNER,CSCANNER                                                
         MVC   ACALLOV,CCALLOV                                                  
         MVC   ALOCKSPC,CLOCKSPC                                                
         MVC   ADMGR,CDATAMGR                                                   
*                                                                               
         L     RF,VSSB             EXTRACT SSB DATA                             
         USING SSBD,RF                                                          
         MVC   RECLEN,SSBTWAL                                                   
         MVC   FACID,SSBSYSN4                                                   
         MVC   FACNA,SSBSYSNA                                                   
         MVC   ALANG,SSBALANG                                                   
         MVC   ALET,SSBTBLET                                                    
         DROP  RF                                                               
*                                                                               
         MVC   SRVHDR,FLHEADER                                                  
         MVC   SRVHDR1,FLUNDER                                                  
*                                                                               
         BRAS  RE,READSTR          READ SAVED VALUES                            
         BRAS  RE,ELOCKSPC         GET VERSION TABLE INFO                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE 1ST PARAMETER FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
P1VAL    NTR1  ,                                                                
         LA    R2,SRVP1H                                                        
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         CLI   FHIL,0              FIELD IS OPTIONAL                            
         BE    EXITOK                                                           
*                                                                               
         CLI   FHDA,C'?'           HELP REQUESTED?                              
         BNE   P1V02                                                            
*                                                                               
         MVC   HELPKEY,HELPID                                                   
         MVI   HELPNUM,1                                                        
         GOTO1 VGETHELP,DMCB,(X'50',HELPKEY),0,(C'B',0),0,0                     
         DC    H'0'                                                             
*                                                                               
P1V02    TM    FHII,FHIINU         NUMERIC?                                     
         BO    *+14                YES                                          
         MVC   FERN,=AL2(FVFNUM)                                                
         B     EXITL                                                            
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FHIL                                                          
         BCTR  RF,0                                                             
         EX    RF,P1VPCK                                                        
         CVB   RF,DUB                                                           
         BCTR  RF,0                                                             
         STC   RF,PAGE             SAVE IN PAGE                                 
         B     EXITOK                                                           
*                                                                               
P1VPCK   PACK  DUB,FHDA(0)                                                      
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE 2ND PARAMETER FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
P2VAL    NTR1  ,                                                                
         LA    R2,SRVP2H                                                        
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         CLI   FHIL,0              INPUT IS OPTIONAL                            
         BE    EXITOK                                                           
*                                                                               
         CLI   FHDA,C'?'           HELP REQUESTED?                              
         BNE   P2V02                                                            
         MVC   HELPKEY,HELPID                                                   
         MVI   HELPNUM,2                                                        
         GOTO1 VGETHELP,DMCB,(X'50',HELPKEY),0,(C'B',0),0,0                     
         DC    H'0'                                                             
*                                                                               
P2V02    MVC   GTSYSL,FHIL         VALIDATE SYSTEM NAME                         
         MVC   GTSYSN,FHDA                                                      
         BAS   RE,VALOVS                                                        
         BE    *+14                                                             
         MVC   FERN,=AL2(FVFISYS)                                               
         B     EXITL                                                            
*                                                                               
         MVC   FLSYS,GTSYS         SET SYSTEM FILTER                            
         CLC   OFLSYS,FLSYS                                                     
         BE    EXITOK                                                           
         OI    DDS,DDSNEW          SET NEW SCREEN IF FLSYS CHANGES              
         MVC   OFLSYS,FLSYS                                                     
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE 3RD PARAMETER FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
P3VAL    NTR1  ,                                                                
         LA    R2,SRVP3H                                                        
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         CLI   FHIL,0              INPUT IS OPTIONAL                            
         BE    EXITOK                                                           
*                                                                               
         CLI   FHDA,C'?'           HELP REQUESTED?                              
         BNE   P3V02                                                            
         MVC   HELPKEY,HELPID                                                   
         MVI   HELPNUM,3                                                        
         GOTO1 VGETHELP,DMCB,(X'50',HELPKEY),0,(C'B',0),0,0                     
         DC    H'0'                                                             
*                                                                               
P3V02    LA    R5,INPENTRY         SPECIAL OPERATOR INPUT FIELD                 
         USING VRSNTABD,R5                                                      
         NI    DDS,255-DDSDEL      ASSUME NO SPECIAL OPERATOR DELETE            
         GOTO1 VSCANNER,DMCB,FHD,IOAREA,C',=/ '                                 
         CLI   DMCB+4,4                                                         
         BNE   EXITNV              MUST BE FORMAT ROOT/ADV/SENAME/UPDN          
         CLI   IOAREA+12,C'T'      FORMAT TXXX?                                 
         BNE   EXITNV              NO                                           
         MVC   FULL,IOAREA+12      FROM 1ST SCANNER TABLE ROW                   
         MVI   FULL,C'0'           REPLACE 'T' WITH '0'                         
         GOTO1 VHEXIN,DMCB,FULL,VRSNPGM,4                                       
         CLC   =F'2',12(R1)                                                     
         BNE   EXITNV              INVALID INPUT                                
         CLI   IOAREA+32,0         ADV GIVEN?                                   
         BE    P3VAL10             NO                                           
         MVC   GTADVN,IOAREA+44    FROM 2ND SCANNER TABLE ROW                   
         BAS   RE,VALADV           VALIDATE ADV NAME                            
         BNE   EXITNV              INVALID INPUT                                
         MVC   VRSNADV,GTADV       VALID -- SAVE ADV NUMBER                     
         B     P3VAL20                                                          
P3VAL10  CLI   IOAREA+64,0         SENAME GIVEN?                                
         BE    EXITNV              NO: REQUIRED IF ADV IS ABSENT                
         MVC   GTSYSN,IOAREA+76    FROM 3RD SCANNER TABLE ROW                   
         BAS   RE,VALSYS           VALIDATE SYSTEM NAME                         
         BNE   EXITNV                                                           
         MVC   VRSNSEN,GTSYSSE     VALID -- SAVE SENUM                          
P3VAL20  CLC   =C'UP',IOAREA+108   FROM 4TH SCANNER TABLE ROW                   
         BE    P3VAL30             'UP': NO FLAG SETTING NECESSARY              
         CLC   =C'DN',IOAREA+108                                                
         BNE   *+12                                                             
         OI    VRSNFLAG,VRSNNOOP   'DN': TURN ON NO-OP FLAG                     
         B     P3VAL30                                                          
         CLC   =C'DD',IOAREA+108   'DD': DELETE ENTRY FROM TABLE                
         BNE   EXITNV                                                           
         OI    DDS,DDSDEL          REMEMBER: OPERATOR DELETE (VIA P3)           
         B     P3VALXX                                                          
         DROP  R5                                                               
P3VAL30  MVC   VIRGIN,INPENTRY                                                  
         BAS   RE,DUPES            CHECK FOR DUPLICATES                         
         BE    *+14                                                             
         MVC   FERN,=AL2(FVFDUP)                                                
         B     EXITL                                                            
*                                                                               
         XC    SRVP3,SRVP3         CLEAR INPUT FIELD IF VALID                   
P3VALXX  EQU   *                                                                
         SPACE 1                                                                
PARMVALX B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DEAL WITH SCROLLING                                                 *         
***********************************************************************         
         SPACE 1                                                                
SCROLL   NTR1  ,                                                                
         XR    R1,R1                                                            
         TM    DDS,DDSNEW          RESET PAGE IF NEW                            
         BO    SCRL02                                                           
*                                                                               
         IC    R1,PAGE             GET PAGE NUMBER                              
         CLI   PFKEY,8                                                          
         BH    SCRL02                                                           
         CLI   PFKEY,7                                                          
         BL    SCRL02                                                           
*                                                                               
         AHI   R1,1                ASSUME DOWN                                  
         CLI   PFKEY,7                                                          
         BNE   *+8                                                              
         AHI   R1,-2               UP 1 PLUS CORRECTION FOR ASSUMPTION          
         LTR   R1,R1                                                            
         BNM   *+6                                                              
         XR    R1,R1               CLEAR R1 IF IT GOES NEGATIVE                 
*                                                                               
SCRL02   STC   R1,PAGE                                                          
         CLC   PAGE,SVPAGE         NEW SCREEN IF PAGE CHANGED                   
         BE    *+8                                                              
         OI    DDS,DDSNEW                                                       
         MVC   SVPAGE,PAGE         SAVE THIS PAGE VALUE                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* READ TWA 11                                                         *         
***********************************************************************         
         SPACE 1                                                                
READSTR  NTR1  ,                                                                
         XC    DMCB(24),DMCB       READ TWA 11 FOR THIS TERMINAL                
         LHI   R0,SRPAGENO                                                      
         SLL   R0,32-8                                                          
         ICM   R0,3,TRM                                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 ADMGR,DMCB,(0,DMREAD),TEMPSTR,(R0),ATIA                          
         CLI   8(R1),0                                                          
         BE    *+6                 SHOULDN'T BE ANY ERRORS                      
         DC    H'0'                                                             
*                                                                               
         L     RE,ATIA             COPY SAVED DATA FROM PAGE 11                 
         LA    RE,SR$VRSN-SRSD(RE)                                              
         LA    R0,SAVED                                                         
         LHI   R1,SAVEDLQ                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         TM    DDS,DDSNEW          IF FIRST TIME ALWAYS CLEAR HEADER            
         BO    *+14                                                             
         CLC   VERWORD,VERHDR      DID I PUT SOMETHING IN TWA 11?               
         BE    EXITOK              NO                                           
*                                                                               
         LA    R0,SAVED            CLEAR HEADER                                 
         LHI   R1,SAVEDLQ                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* WRITE TWA 11                                                        *         
***********************************************************************         
         SPACE 1                                                                
WRITESTR NTR1  ,                                                                
         MVC   VERWORD,VERHDR      SET OUR INDICATOR                            
         MVC   SVTIME,MYTIME       SAVE LAST DISPLAY TIME                       
*                                                                               
         XC    DMCB(24),DMCB       READ TWA 11 FOR UPDATE                       
         LHI   R0,SRPAGENO                                                      
         SLL   R0,32-8                                                          
         ICM   R0,3,TRM                                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 ADMGR,DMCB,(X'80',DMREAD),TEMPSTR,(R0),ATIA                      
         CLI   8(R1),0                                                          
         BE    *+6                 SHOULDN'T BE ANY ERRORS                      
         DC    H'0'                                                             
*                                                                               
         L     RE,ATIA             MOVE BACK SAVED STORAGE                      
         LA    RE,SR$VRSN-SRSD(RE)                                              
         LA    R0,SAVED                                                         
         LHI   RF,L'SRCOMWRK                                                    
         LHI   R1,SAVEDLQ                                                       
         MVCL  RE,R0                                                            
*                                                                               
         LA    R0,SRPAGENO         WRITE BACK TWA 11                            
         SLL   R0,32-8                                                          
         ICM   R0,3,TRM                                                         
         GOTO1 ADMGR,DMCB,(0,DMWRT),TEMPSTR,(R0),ATIA                           
         CLI   8(R1),0                                                          
         BE    EXITOK                                                           
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* CLEAR ALL DISPLAY FIELDS FOR REDISPLAY                              *         
***********************************************************************         
         SPACE 1                                                                
CLRSCRN  NTR1  ,                                                                
         TWAXC SRVSELH,PROT=N,TRNS=Y                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY VERSION TABLE ENTRY                                         *         
* NTRY: R6     = A(SCREEN LINE TO USE)                                *         
*       VIRGIN = A(VERSION TABLE ENTRY)                               *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING VERLINED,R6         OUTPUT LINE ON SCREEN                        
         USING VRSNTABD,VIRGIN     COPY OF VERSION TAB ENTRY                    
VIRDISP  NTR1  ,                                                                
         TM    VRSNPHS,X'80'       TEST PHASE ENTRY                             
         BO    VVD06                                                            
*                                                                               
         GOTO1 VHEXOUT,DMCB,VRSNPGM,VLROOT,2                                    
         MVI   VLROOT,C'T'         ROOT PHASE TSPP                              
*                                                                               
         MVC   GTSYS,VRSNPGM                                                    
         BRAS  RE,GETOVS           GET SYSTEM NAME                              
         BNE   VVD04                                                            
         MVC   VLSYSP(3),GTSYSN                                                 
         MVI   VLSYSP+3,C'/'                                                    
*                                                                               
VVD02    MVI   GTSYSSE,0                                                        
         BAS   RE,GETSYS           GET A(PGMLST)                                
         BNE   VVD04                                                            
*                                                                               
         MVC   GTPROG,VRSNPGM+1                                                 
         BAS   RE,GETPROG          GET PROGRAM NAME                             
         MVC   VLSYSP+4(3),GTPROGN MAKE IT SYS/PRG                              
*                                                                               
VVD04    MVC   GTLANG,VRSNFLAG     EXTRACT LANG FROM FLAGS                      
         NI    GTLANG,X'0F'                                                     
         BAS   RE,GETLANG          GET LANGUAGE                                 
         MVC   VLLANG,GTLANGN                                                   
*                                                                               
         TM    VRSNFLAG,VRSNDATE   TEST PAST EXPIRY DATE                        
         BNO   *+8                                                              
         MVI   VLFLAGS,C'*'        SET * IN FLAGS                               
         B     VVD08                                                            
*                                                                               
VVD06    MVC   VLROOT,=C'PHS='                                                  
         GOTO1 VHEXOUT,DMCB,VRSNPHS,VLSYSP,3                                    
         MVI   VLSYSP,C'T'         ROOT PHASE TSPP                              
*                                                                               
VVD08    MVC   VLADV,SPACES                                                     
         CLI   VRSNADV,0                                                        
         BE    VVD10                                                            
         MVC   GTADV,VRSNADV                                                    
         BAS   RE,GETADV           GET ADV NAME                                 
         MVC   VLADV,GTADVN                                                     
*                                                                               
VVD10    MVC   VLSENUM,SPACES                                                   
         CLI   VRSNSEN,0                                                        
         BE    VVD12                                                            
         MVC   GTSYSSE,VRSNSEN                                                  
         BAS   RE,GETSYS           GET SE SYSTEM NAME                           
         MVC   VLSENUM,GTSYSN                                                   
*                                                                               
VVD12    MVC   VLAGY,SPACES        2 CHR AGENCY                                 
         CLI   VLAGY,0                                                          
         BE    VVD14                                                            
         MVC   VLAGY,VRSNAGY                                                    
*                                                                               
VVD14    TM    VRSNPHS,X'80'                                                    
         BO    VVD16                                                            
         MVC   VLLEVEL,=C'UP'                                                   
         TM    VRSNFLAG,VRSNNOOP   CHECK PROGRAM NO-OP FLAG                     
         BZ    VVD16                                                            
         MVC   VLLEVEL,=C'DN'                                                   
         B     EXITOK                                                           
*                                                                               
VVD16    CLI   VRSNABC,0           ANY VERSION TO DISPLAY?                      
         BE    EXITOK              NO                                           
         MVC   VLLEVEL(1),VRSNABC  LEVEL 1/2/3                                  
         OI    VLLEVEL,X'C0'       SET TO A/B/C                                 
         MVI   VLLEVEL+1,C' '                                                   
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE VERSION TABLE ENTRY                                        *         
* NTRY: R6     = A(SCREEN LINE TO USE)                                *         
*       VIRGIN = A(VERSION TABLE ENTRY)                               *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING VERLINED,R6         OUTPUT LINE ON SCREEN                        
         USING VRSNTABD,VIRGIN     COPY OF VERSION TAB ENTRY                    
VIRVAL   NTR1  ,                                                                
         MVC   VSAVE,VIRGIN        SAVE OLD VIRGIN                              
         MVC   TSPP1,VIRGIN        SAVE OLD TSPP                                
         XC    TSPP2,TSPP2                                                      
         XC    TSPP3,TSPP3                                                      
*                                                                               
         XC    VIRGIN,VIRGIN       XC FOR A VIRGIN VIRGIN                       
         CLI   ACTN,C'D'           TEST FOR DELETE                              
         BE    EXITOK                                                           
*                                                                               
         LA    R2,VLROOTH                                                       
         USING FHD,R2                                                           
         ST    R2,FADRH                                                         
         CLI   FHIL,0              TEST FOR NO INPUT                            
         BE    VVL02                                                            
*                                                                               
         CLC   =C'PHS',FHDA        PHASE ENTRY                                  
         BE    VVL12                                                            
         CLI   FHDA,C'T'           ROOT PHASE TSPP                              
         BNE   EXITNV                                                           
         MVC   FULL,FHDA                                                        
         MVI   FULL,C'0'                                                        
         GOTO1 VHEXIN,DMCB,FULL,TSPP2,4                                         
         OC    12(4,R1),12(R1)                                                  
         BZ    EXITNV                                                           
*                                                                               
VVL02    LA    R2,VLSYSPH                                                       
         ST    R2,FADRH                                                         
         CLI   FHIL,0              TEST FOR NO INPUT                            
         BE    VVL06                                                            
*                                                                               
         MVC   GTSYSN(3),VLSYSP                                                 
         MVI   GTSYSL,3                                                         
         BAS   RE,VALOVS           VALIDATE SYSTEM NAME                         
         BE    *+14                                                             
         MVC   FERN,=AL2(FVFISYS)                                               
         B     EXITL                                                            
*                                                                               
         MVC   TSPP3(1),GTSYS                                                   
         CLI   VLSYSP+3,C'/'                                                    
         BE    VVL04                                                            
         MVI   FERRDSP,3                                                        
         B     EXITNV                                                           
*                                                                               
VVL04    MVI   GTSYSSE,0                                                        
         BRAS  RE,GETSYS           GET A(PGMLST)                                
         BNE   EXITNV                                                           
         MVC   GTPROGN(3),VLSYSP+4                                              
         MVI   GTPROGL,3                                                        
         BAS   RE,VALPROG          VALIDATE PROGRAM NAME                        
         BE    *+12                                                             
         MVI   FERRDSP,4                                                        
         B     EXITNV                                                           
*                                                                               
         MVC   TSPP3+1(1),GTPROG                                                
*                                                                               
VVL06    CLC   TSPP2,TSPP3         DO WE AGREE                                  
         BE    VVL10                                                            
         OC    TSPP3,TSPP3         IF 3 IS ZERO 2 WINS                          
         BZ    VVL08                                                            
         OC    TSPP2,TSPP2         IF 2 IS ZERO 3 WINS                          
         BZ    VVL10                                                            
         CLC   TSPP2,TSPP1         IF 2 IS SAME AS OLD 3 WINS                   
         BE    VVL10                                                            
         CLC   TSPP3,TSPP1         IF 3 IS SAME AS OLD 2 WINS                   
         BE    VVL08                                                            
*                                                                               
         MVC   VRSNPGM,TSPP1       IF ALL ELSE FAILS OLD WINS                   
         B     VVL14                                                            
*                                                                               
VVL08    MVC   VRSNPGM,TSPP2                                                    
         B     VVL14                                                            
*                                                                               
VVL10    MVC   VRSNPGM,TSPP3                                                    
         B     VVL14                                                            
*                                                                               
VVL12    MVC   DUB,VLSYSP          VALIDATE PHASE IN SYS/PRG                    
         MVI   DUB,C'0'                                                         
         GOTO1 VHEXIN,DMCB,DUB,VRSNPHS,6                                        
         OC    12(4,R1),12(R1)                                                  
         BZ    EXITNV              INVALID INPUT                                
         OI    VRSNPHS,X'80'                                                    
         BRAS  RE,SETFLAGS                                                      
         B     VVL16                                                            
*                                                                               
VVL14    MVC   VRSNFLAG,VSAVE+(VRSNFLAG-VRSNPGM)                                
         NI    VRSNFLAG,X'F0'      COPY FLAGS FROM OLD ENTRY                    
*                                                                               
         LA    R2,VLLANGH                                                       
         ST    R2,FADRH                                                         
         CLI   FHIL,0              TEST FOR NO INPUT                            
         BE    VVL16                                                            
*                                                                               
         MVC   GTLANGN,VLLANG                                                   
         BRAS  RE,VALLANG          VALIDATE LANGUAGE                            
         BE    *+14                INVALID INPUT                                
         MVC   FERN,=AL2(FVFILNG)                                               
         B     EXITL                                                            
*                                                                               
         OC    VRSNFLAG,GTLANG                                                  
*                                                                               
VVL16    LA    R2,VLADVH           ADV NAME                                     
         ST    R2,FADRH                                                         
         CLI   FHIL,0                                                           
         BE    VVL18                                                            
         MVC   GTADVN,VLADV                                                     
         BRAS  RE,VALADV                                                        
         BNE   EXITNV              INVALID INPUT                                
         MVC   VRSNADV,GTADV                                                    
*                                                                               
VVL18    LA    R2,VLSENUMH                                                      
         ST    R2,FADRH                                                         
         CLI   FHIL,0                                                           
         BE    VVL20                                                            
         MVC   GTSYSN,FHDA                                                      
         OC    GTSYSN,SPACES                                                    
         BRAS  RE,VALSYS           VALIDATE SYSTEM NAME                         
         BE    *+14                INVALID INPUT                                
         MVC   FERN,=AL2(FVFISYS)                                               
         B     EXITL                                                            
*                                                                               
         MVC   VRSNSEN,GTSYSSE                                                  
*                                                                               
VVL20    LA    R2,VLAGYH           VALIDATE AGENCY                              
         ST    R2,FADRH                                                         
         CLI   FHIL,0                                                           
         BE    VVL22                                                            
         MVC   VRSNAGY,VLAGY                                                    
*                                                                               
VVL22    LA    R2,VLLEVELH                                                      
         ST    R2,FADRH                                                         
         CLI   FHIL,0                                                           
         BE    VVL28               MISSING INPUT                                
*                                                                               
         MVI   VRSNABC,0           RESET LEVEL A/B/C                            
*                                                                               
         TM    VRSNPHS,X'80'       DON'T TOUCH FLAGS ON THESE                   
         BO    VVL24                                                            
         NI    VRSNFLAG,255-VRSNNOOP                                            
         CLC   VLLEVEL,=C'UP'      EXPLICIT COMMAND: OPERATIONAL                
         BE    VVL26                                                            
         CLC   VLLEVEL,=C'DN'      EXPLICIT COMMAND: NO-OP                      
         BNE   *+12                                                             
         OI    VRSNFLAG,VRSNNOOP                                                
         B     VVL26                                                            
*                                                                               
VVL24    CLI   VLLEVEL,C'A'                                                     
         BL    EXITNV                                                           
         CLI   VLLEVEL,C'C'                                                     
         BH    EXITNV                                                           
         MVC   VRSNABC,VLLEVEL     LEVEL A/B/C                                  
         NI    VRSNABC,255-X'C0'   CHANGE TO 1/2/3                              
*                                                                               
VVL26    LA    R2,VLSYSPH                                                       
         ST    R2,FADRH                                                         
         OC    VRSNPGM,VRSNPGM     MUST HAVE PRGM                               
         BZ    EXITNO                                                           
*                                                                               
         CLC   VIRGIN,VSAVE        IF SOMETHING CHANGED                         
         BE    VVL30                                                            
         BAS   RE,DUPES            CHECK FOR DUPLICATES                         
         BE    VVL30                                                            
         MVC   FERN,=AL2(FVFDUP)                                                
         B     EXITL                                                            
*                                                                               
VVL28    OC    VIRGIN,VIRGIN       TEST FOR NO INPUT                            
         BZ    VVLX                                                             
         B     EXITNO              ELSE MISSING INPUT                           
         EJECT                                                                  
*                                                                               
VVL30    TM    DDS,DDSDEL          IS OPERATOR DOING A DELETE (VIA P3)?         
         BZ    VVLX                NO                                           
*                                                                               
INPENT   USING VRSNTABD,RF                                                      
         LA    RF,INPENTRY                                                      
         CLC   VRSNPGM,INPENT.VRSNPGM  DOES IT MATCH THE CURRENT ENTRY?         
         BNE   VVLX                                                             
         CLC   VRSNADV,INPENT.VRSNADV                                           
         BNE   VVLX                                                             
         CLC   VRSNSEN,INPENT.VRSNSEN                                           
         BNE   VVLX                                                             
         CLC   VRSNAGY,INPENT.VRSNAGY                                           
         BNE   VVLX                                                             
         CLC   VRSNABC,INPENT.VRSNABC                                           
         BNE   VVLX                                                             
         DROP  INPENT                                                           
         XC    VIRGIN,VIRGIN       YES: DELETE IT                               
*                                                                               
VVLX     B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* SET PHASE LIST FLAGS                                                *         
* NTRY: R4     = A(VERSION TABLE ENTRY)                               *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING VERLINED,R6         OUTPUT LINE ON SCREEN                        
         USING VRSNTABD,VIRGIN     COPY OF VERSION TAB ENTRY                    
SETFLAGS NTR1  ,                                                                
         L     R2,AUTL                                                          
         USING UTLD,R2                                                          
         MVC   STEST,TTEST                                                      
         MVC   STEST1,TTEST1                                                    
         MVC   SLANG,TLANG                                                      
         MVI   TLANG,0             SET LANGUAGE CODE                            
*                                                                               
         MVC   BYTE,VLLEVEL                                                     
         NI    BYTE,TTESTLVL                                                    
         NI    TTEST,255-TTESTLVL                                               
         OI    TTEST1,TTESTURT     SET CALLOV FLAG TO USE REAL TTEST            
*NOPAHYD OI    TTEST,TTESTSRA      *TEMP*                                       
         OC    TTEST,BYTE          SET PHASE LIST LEVEL                         
*                                                                               
         MVC   FULL(3),VRSNPHS                                                  
         NC    FULL(3),=XL3'7FFFFF'                                             
         ICM   R0,7,FULL                                                        
         ICM   R0,8,=C'S'                                                       
         GOTO1 ACALLOV,DMCB,PSREC,(R0),(C'D',0)                                 
         CLI   4(R1),255                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TTEST,STEST         RESET READ INDICATOR                         
         MVC   TTEST1,STEST1                                                    
         MVC   TLANG,SLANG         RESET LANGUAGE CODE                          
*                                                                               
         TM    PSREC+PSFLAG1-PROGSPCD,CTPHSCRQ                                  
         BZ    EXITOK                                                           
*                                                                               
         L     RF,VSSB             SET CORERES VERSION CONTROL FLAG             
         OI    SSBSTAT4-SSBD(RF),SSBVCNTL                                       
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DO FILTERING                                                        *         
* NTRY:  VIRGIN  = A(COPY OF TABLE ENTRY)                             *         
* EXIT:  CC:NE   = DO NOT SHOW ENTRY                                  *         
***********************************************************************         
         SPACE 1                                                                
FILTERS  NTR1  ,                                                                
         LA    R2,VIRGIN                                                        
         USING VRSNTABD,R2         COPY OF VERSION TAB ENTRY                    
         CLI   VRSNPGM,0                                                        
         BE    EXITOK              NULL ENTRY                                   
         CLI   FLSYS,0                                                          
         BE    EXITOK              NO FILTER                                    
         CLC   FLSYS,VRSNPGM                                                    
         BE    EXITOK              FILTER MATCHES                               
         B     EXITL                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET GTSYS & GTSYSN FROM GTSYSSE                                     *         
***********************************************************************         
         SPACE 1                                                                
GETSYS   NTR1  ,                                                                
         L     R2,VSELIST                                                       
         LH    RE,0(R2)                                                         
         L     RF,2(R2)                                                         
         AHI   R2,6                                                             
         USING SELISTD,R2                                                       
*                                                                               
GTSYS02  CLI   GTSYSSE,0           IF SYSSE IS ZERO                             
         BNE   *+14                                                             
         CLC   GTSYS,SEOVSYS       TEST GTSYS WITH OVSYS                        
         BE    GTSYS04                                                          
*                                                                               
         CLC   GTSYSSE,SESYS       ELSE TEST SE NUMBER                          
         BE    GTSYS04                                                          
         BXLE  R2,RE,GTSYS02       NEXT                                         
         MVI   GTSYS,0                                                          
         XC    GTSYSN,GTSYSN                                                    
         B     EXITL                                                            
*                                                                               
GTSYS04  MVC   GTSYS,SEOVSYS                                                    
         MVC   GTSYSN,SENAME       SET NAME                                     
         MVC   GTAPGMS,SEPGMS      SAVE A(PGMS)                                 
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET GTSYSSE FROM GTSYSN                                             *         
***********************************************************************         
         SPACE 1                                                                
VALSYS   NTR1  ,                                                                
         L     R2,VSELIST          MUST HAVE SYSFACS                            
         LH    RE,0(R2)            SET BXLE                                     
         L     RF,2(R2)                                                         
         AHI   R2,6                                                             
         USING SELISTD,R2                                                       
*                                                                               
         CLC   GTSYSN,SENAME       TEST NAME                                    
         BE    VLSYS02                                                          
         BXLE  R2,RE,*-10          NEXT                                         
         MVI   GTSYS,0                                                          
         XC    GTSYSN,GTSYSN                                                    
         B     EXITL               ERROR EXIT NOT FOUND                         
*                                                                               
VLSYS02  MVC   GTSYS,SEOVSYS       FOUND                                        
         MVC   GTSYSSE,SESYS       SET NUMBER                                   
         MVC   GTAPGMS,SEPGMS      SAVE A(PGMS)                                 
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET GTPROGN FROM GTPROG                                             *         
***********************************************************************         
         SPACE 1                                                                
GETPROG  NTR1  ,                                                                
         ICM   R2,15,GTAPGMS       MUST HAVE A(PRGMS)                           
         BZ    GTPRG02                                                          
         LH    RE,0(R2)            SET BXLE                                     
         L     RF,2(R2)                                                         
         AHI   R2,6                                                             
         USING PGMLSTD,R2                                                       
*                                                                               
         CLC   GTPROG,PGMNUM       TEST PROG NUMBER                             
         BE    GTPRG04                                                          
         BXLE  R2,RE,*-10          NEXT                                         
*                                                                               
GTPRG02  MVC   GTPROGN,SPACES      NOT FOUND SO HEXOUT NUMBER                   
         GOTO1 VHEXOUT,DMCB,GTPROG,GTPROGN,1                                    
         B     EXITOK                                                           
*                                                                               
GTPRG04  MVC   GTPROGN,PGMNAME     SET NAME                                     
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET GTPROG FROM GTPROGN                                             *         
***********************************************************************         
         SPACE 1                                                                
VALPROG  NTR1  ,                                                                
         CLI   GTPROGN+2,C' '      FIX FOR 2CHR PROG NAMES                      
         BH    *+8                                                              
         MVI   GTPROGN+2,C' '                                                   
*                                                                               
         XR    R1,R1                                                            
         IC    R1,GTPROGL                                                       
         BCTR  R1,0                                                             
*                                                                               
         ICM   R2,15,GTAPGMS       MUST HAVE A(PGMS)                            
         BNZ   *+14                                                             
         XC    GTPROGN,GTPROGN     NOT FOUND                                    
         B     EXITL                                                            
*                                                                               
         LH    RE,0(R2)            SET BXLE                                     
         L     RF,2(R2)                                                         
         AHI   R2,6                                                             
         USING PGMLSTD,R2                                                       
*                                                                               
         EX    R1,VPRGCLC          TRY TO MATCH NAME                            
         BE    VLPRG02                                                          
         BXLE  R2,RE,*-8                                                        
*                                                                               
         GOTO1 VHEXIN,DMCB,GTPROGN,GTPROG,2                                     
         OC    12(4,R1),12(R1)     TRY FOR HEX LAST                             
         BNZ   EXITOK                                                           
         XC    GTPROGN,GTPROGN     NOT FOUND                                    
         B     EXITL                                                            
*                                                                               
VLPRG02  MVC   GTPROG,PGMNUM       FOUND                                        
         B     EXITOK                                                           
*                                                                               
VPRGCLC  CLC   GTPROGN(0),PGMNAME  TEST NAME                                    
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET GTLANGN FROM GTLANG                                             *         
***********************************************************************         
         SPACE 1                                                                
GETLANG  NTR1  ,                                                                
         L     R2,ALANG            MUST HAVE A(LANGTAB)                         
         LH    RE,0(R2)            SET BXLE                                     
         L     RF,2(R2)                                                         
         AHI   R2,6                                                             
         USING LANGTABD,R2                                                      
         CLC   GTLANG,LANGCODE     TEST LANGUAGE CODE                           
         BE    GTLAN02                                                          
         BXLE  R2,RE,*-10          NEXT                                         
         MVC   GTLANGN,SPACES                                                   
         B     EXITL               ERROR EXIT NOT FOUND                         
*                                                                               
GTLAN02  MVC   GTLANGN,LANGSHR     SET NAME                                     
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET GTLANG FROM GTLANGN                                             *         
***********************************************************************         
         SPACE 1                                                                
VALLANG  NTR1  ,                                                                
         MVI   GTLANG,0                                                         
         CLC   GTLANGN,SPACES      MAKE SURE FIELD HAS INPUT                    
         BNH   EXITL                                                            
*                                                                               
         LA    R1,GTLANGN+L'GTLANGN-1                                           
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    RF,GTLANGN                                                       
         SR    R1,RF               R1 = L'GTLANG INPUT - 1                      
*                                                                               
         L     R2,ALANG            MUST HAVE A(LANGTAB)                         
         LH    RE,0(R2)            SET BXLE                                     
         L     RF,2(R2)                                                         
         AHI   R2,6                                                             
         USING LANGTABD,R2                                                      
*                                                                               
         EX    R1,VLANCLC          TEST NAME                                    
         BE    VLAN02                                                           
         BXLE  R2,RE,*-8                                                        
         B     EXITL                                                            
*                                                                               
VLAN02   MVC   GTLANG,LANGCODE     FOUND                                        
         B     EXITOK                                                           
*                                                                               
VLANCLC  CLC   GTLANGN(0),LANGSHR  TEST NAME                                    
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET GTSYS FROM SYSTEM NAME                                          *         
***********************************************************************         
         SPACE 1                                                                
VALOVS   NTR1  ,                                                                
         LA    R2,SYSLST           R2=SYSLST                                    
         LH    RE,0(R2)                                                         
         L     RF,2(R2)                                                         
         A     RF,RELO                                                          
         AHI   R2,6                                                             
         USING SYSLSTD,R2                                                       
         XR    R1,R1               R1=LEN-1 FOR COMPARE                         
         IC    R1,GTSYSL                                                        
         BCTR  R1,0                                                             
         CHI   R1,2                DO WE HAVE >3 CHRS                           
         BH    VALS02                                                           
*                                                                               
         EX    R1,VLSSCLC          TRY TO MATCH SHORT NAME                      
         BE    VALS04                                                           
         BXLE  R2,RE,*-8                                                        
*                                                                               
VALS02   LA    R2,SYSLST           NOW TRY LONG NAME                            
         AHI   R2,6                                                             
         EX    R1,VLSLCLC                                                       
         BE    VALS04                                                           
         BXLE  R2,R2,*-8                                                        
         B     EXITL               NOT FOUND                                    
*                                                                               
VLSSCLC  CLC   SYSLSHRT(0),GTSYSN  TEST SHORT NAME                              
VLSLCLC  CLC   SYSLNAME(0),GTSYSN  TEST LONG NAME                               
*                                                                               
VALS04   MVC   GTSYS,SYSLNUM                                                    
         B     EXITOK              SET CC EQU                                   
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET SYSTEM NAME FROM GTSYS                                          *         
***********************************************************************         
         SPACE 1                                                                
GETOVS   NTR1  ,                                                                
         LA    R2,SYSLST           R2=SYSLST                                    
         LH    RE,0(R2)                                                         
         L     RF,2(R2)                                                         
         A     RF,RELO                                                          
         AHI   R2,6                                                             
         USING SYSLSTD,R2                                                       
*                                                                               
         CLC   GTSYS,SYSLNUM       TEST SYSTEM NUMBER                           
         BE    GETS02                                                           
         BXLE  R2,RE,*-10                                                       
         B     EXITL                                                            
*                                                                               
GETS02   MVC   GTSYSN,SYSLSHRT                                                  
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET SYSTEM NAME FROM GTADV                                          *         
***********************************************************************         
         SPACE 1                                                                
GETADV   NTR1                                                                   
         L     R2,VSSB             GET FACIDTAB FROM SSB                        
         L     R2,SSBAFID-SSBD(R2)                                              
         USING FACITABD,R2                                                      
         LHI   RF,L'FACITAB                                                     
*                                                                               
GADV02   CLI   FACITAB,255         EOT                                          
         BE    EXITL                                                            
         CLC   GTADV,FACIID        MATCH ADV NUMBER                             
         BE    *+8                                                              
         BXH   R2,RF,GADV02                                                     
*                                                                               
         MVC   GTADVN,FACISN4      GET NAME                                     
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET SYSTEM GTADV FROM SYSTEM NAME                                   *         
***********************************************************************         
         SPACE 1                                                                
VALADV   NTR1  ,                                                                
         OC    GTADVN,SPACES       FIX FOR SHORT ADV NAMES                      
*                                                                               
         L     R2,VSSB             GET FACIDTAB FROM SSB                        
         L     R2,SSBAFID-SSBD(R2)                                              
         USING FACITABD,R2                                                      
         LHI   RF,L'FACITAB                                                     
*                                                                               
VADV02   CLI   FACITAB,255         EOT                                          
         BE    EXITL                                                            
         CLC   GTADVN,FACISN4      MATCH NAME                                   
         BE    *+8                                                              
         BXH   R2,RF,VADV02                                                     
*                                                                               
         MVC   GTADV,FACIID        GET ADV NUMBER                               
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SQUASH OUT ZERO ENTRIES FROM VRSNTAB                                *         
***********************************************************************         
         SPACE 1                                                                
SQUASH   NTR1  ,                                                                
         BRAS  RE,LOCKSPC          LOCK DATASPACE                               
*                                                                               
         XC    ALAST,ALAST                                                      
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR1,AR1,ALET                                                     
         L     R1,VVRSNTAB                                                      
         USING DMSPACED,R1                                                      
         SAC   512                                                              
         XR    R6,R6                                                            
         ICM   R6,3,DSPTWIDE       SET BXLE ON R6/7                             
         ICM   R7,15,DSPTEND                                                    
         ICM   R1,15,DSPTFRST      SET CURRENT POINT IN R6                      
         LA    R1,0(R1)            AND CLEAR HOB                                
         USING VRSNTABD,R1                                                      
*                                                                               
SQUASH04 OC    VRSNPGM,VRSNPGM     TEST FOR EMPTY SLOT                          
         BNZ   SQUASH06            NOT EMPTY                                    
         OC    ALAST,ALAST         ALREADY HAVE EMPTY SLOT?                     
         BNZ   SQUASH08            YES                                          
         ST    R1,ALAST            NO - SET THIS AS FIRST EMPTY SLOT            
         B     SQUASH08                                                         
*                                                                               
SQUASH06 OC    ALAST,ALAST         DO WE HAVE AN EMPTY SLOT TO FILL?            
         BZ    SQUASH08            NO                                           
*                                                                               
         CPYA  AR2,AR1             SET UP MVCL TO R2/3 FROM R4/5                
         L     R2,ALAST            R2=TO ADDRESS                                
         LA    R3,1(R7)            R3=A(EOT)                                    
         SR    R3,R2               R3=DISTANCE(EOT-FIRST EMPTY)                 
*                                                                               
         CPYA  ARE,AR1                                                          
         LR    RE,R1               R4=A(CURRENT)                                
         LA    RF,1(R7)            R5=A(EOT)                                    
         SR    RF,RE               R5=DISTANCE(EOT-CURRENT)                     
*                                                                               
         MVCL  R2,RE               MOVE OVER EMPTY SLOTS                        
*                                                                               
         L     R1,ALAST            MAKE SURE ALL IS WELL HERE                   
         XC    ALAST,ALAST         CLEAR A(LAST SPACE)                          
         LAM   AR2,AR2,ARZERO                                                   
         LAM   ARE,ARE,ARZERO      MAKE SURE YOU CLEAR THE ARS                  
*                                                                               
SQUASH08 BXLE  R1,R6,SQUASH04      NEXT                                         
*                                                                               
         SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         BRAS  RE,ULOCKSPC                                                      
         LAM   AR0,ARF,ARZERO                                                   
*                                                                               
         TM    DDS,DDSDEL          IS OPERATOR DOING A DELETE (VIA P3)?         
         BO    EXIT                YES -- NOTHING TO INSERT                     
*                                                                               
         L     R1,ALAST            A(LAST SPACE)                                
         LAM   AR1,AR1,ALET                                                     
         SAC   512                                                              
         MVC   0(8,R1),INPENTRY    INSERT OPERATOR ENTRY (FROM PARAM3)          
         SAC   0                                                                
         LAM   AR1,AR1,ARZERO                                                   
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK FOR DUPLICATES                                                *         
* EXIT   CC:NE = DUPLICATE FOUND                                      *         
***********************************************************************         
         SPACE 1                                                                
DUPES    NTR1  ,                                                                
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,ALET                                                     
         L     R2,VVRSNTAB                                                      
         USING DMSPACED,R2                                                      
         SAC   512                                                              
         XR    RE,RE                                                            
         ICM   RE,3,DSPTWIDE       SET BXLE ON R6/7                             
         ICM   RF,15,DSPTEND                                                    
         ICM   R2,15,DSPTFRST      SET CURRENT POINT IN R6                      
         N     R2,=XL4'0FFFFFFF'                                                
         USING VRSNTABD,R2                                                      
*                                                                               
DUPE02   CLC   VIRGIN,VRSNPGM      TEST FOR DUPLICATES                          
         BE    DUPE04                                                           
         BXLE  R2,RE,DUPE02        NEXT                                         
         SAC   0                   NO DUPLICATES                                
         LAM   AR2,AR2,ARZERO                                                   
         B     EXITOK                                                           
*                                                                               
DUPE04   SAC   0                   DUPLICATE FOUND                              
         LAM   AR2,AR2,ARZERO                                                   
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* CALL GETHELP AND EXIT TO MONITOR                                    *         
***********************************************************************         
         SPACE 1                                                                
HELPOUT  L     R1,AHELP                                                         
         OI    6(R1),X'40'         SET CURSOR                                   
         LR    R0,R1                                                            
         MVC   HELPKEY,HELPID                                                   
*                                                                               
         LA    RE,HELPTAB          FIND WHICH PANEL                             
         SR    RF,RF                                                            
         LA    RF,1                                                             
HELP010  EX    0,0(RE)             BY TESTING AHELP                             
         CR    R1,R0                                                            
         BE    HELP020                                                          
         LA    RE,4(RE)                                                         
         CLI   0(RE),0                                                          
         BE    HELP020                                                          
         LA    RF,1(RF)                                                         
         B     HELP010                                                          
*                                                                               
HELP020  STC   RF,HELPNUM                                                       
         XC    DMCB(24),DMCB                                                    
         GOTO1 VGETHELP,DMCB,(X'50',HELPKEY),0,(C'B',0)                         
         DC    H'0'                                                             
*                                                                               
HELPTAB  LA    R1,SRVP1H           POSSIBLE HELP FIELDS                         
         LA    R1,SRVP2H                                                        
         LA    R1,SRVP3H                                                        
         LA    R1,SRVP4H                                                        
         LA    R1,SRVSELH          <--- OR GREATER                              
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY ERROR MESSAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
DISERR   NTR1  ,                                                                
         XC    DMCB(24),DMCB       R0 HAS SERVICE SYSTEM ERR NUM                
         LH    R0,FERN                                                          
         GOTO1 VGETTXT,DMCB,(R0),0,(C'E',0),(4,FACID)                           
*                                                                               
         ICM   R2,15,FADRH         SET CURSOR ON BAD FIELD                      
         BZ    EXITOK                                                           
*                                                                               
         L     R3,ATIOB                                                         
         USING TIOBD,R3                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    RF,R2                                                            
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,FERRDSP                                                 
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY OK MESSAGE                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISOK    NTR1  ,                                                                
         XC    DMCB(24),DMCB                                                    
         XR    R0,R0                                                            
         ICM   R0,3,HDRN                                                        
         BZ    DOK02                                                            
         GOTO1 VGETTXT,DMCB,(R0),0,(C'I',0),(4,FACID)                           
*                                                                               
DOK02    L     R3,ATIOB                                                         
         USING TIOBD,R3                                                         
         OI    TIOBINDS,TIOBSETC                                                
         ICM   RF,15,FADRH                                                      
         BNZ   *+8                                                              
         LA    RF,SRVP1H                                                        
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,FERRDSP                                                 
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ENQUIRE ON A DATASPACE ENTRY                             *         
* EXIT: DSPHD  = TABLE HEADER FROM DSPACE                             *         
***********************************************************************         
         SPACE 1                                                                
ELOCKSPC NTR1  ,                                                                
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTVRSN)                                              
         MVI   DUB,X'20'                                                        
         GOTO1 ALOCKSPC,DUB        GET CORRECT TABLE                            
         ICM   RF,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DSPHD,0(RF)         SAVE DSPACE HEADER                           
         NC    DSPTFRST,=XL4'0FFFFFFF'  TURN OFF X'40' POST BIT                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO LOCK A DATASPACE ENTRY                                   *         
* EXIT: DSPHD  = TABLE HEADER FROM DSPACE                             *         
*       YOU HAVE EXCLUSIVE CONTROL UNTIL RELEASED WITH UNLOCK         *         
***********************************************************************         
         SPACE 1                                                                
LOCKSPC  NTR1  ,                                                                
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTVRSN)                                              
         GOTO1 ALOCKSPC,DUB        LOCK CORRECT TABLE                           
         ICM   RF,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DSPHD,0(RF)         SAVE DSPACE HEADER                           
         NC    DSPTFRST,=XL4'0FFFFFFF'  TURN OFF X'40' POST BIT                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UNLOCK A DATASPACE ENTRY                                 *         
* NTRY: LOCKID = TABLE IDENTIFIER                                     *         
***********************************************************************         
         SPACE 1                                                                
ULOCKSPC NTR1  ,                                                                
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTVRSN)                                              
         MVI   DUB,X'10'                                                        
         GOTO1 ALOCKSPC,DUB        UNLOCK CORRECT TABLE                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EXITNV   MVC   FERN,=AL2(FVFNOTV)                                               
         B     EXITL                                                            
*                                                                               
EXITNO   MVC   FERN,=AL2(FVFNONE)                                               
         B     EXITL                                                            
*                                                                               
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
XMOD     L     RD,BASERD                                                        
         B     EXIT                                                             
*                                                                               
ARSOFF   SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         BR    RE                                                               
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
PERPAGE  EQU   32                  # ENTRIES ON A PAGE                          
EMSGL    EQU   45                                                               
IMSGL    EQU   45                                                               
*                                                                               
VERHDR   DC    CL8'$VERSION'                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
DMWRT    DC    CL8'DMWRT   '                                                    
TEMPSTR  DC    CL8'TEMPSTR '                                                    
*                                                                               
FVFNONE  EQU   1                   MISSING INPUT FIELD                          
FVFNOTV  EQU   2                   FIELD IS INVALID                             
FVFNUM   EQU   3                   FIELD MUST BE NUMERIC                        
FVFBIG   EQU   9                   FIELD VALUE IS EXCEEDS MAX ALLOWED           
FVFACT   EQU   21                  INVALID ACTION                               
FVFISYS  EQU   32                  INVALID SYSTEM NAME                          
FVFISUB  EQU   57                                                               
FVFILNG  EQU   67                  INVALID LANGUAGE                             
FVFNUP   EQU   75                  TABLE UPDATED                                
FVFFULL  EQU   76                  TABLE FULL                                   
FVFDUP   EQU   192                                                              
FVFBACT  EQU   253                 INVALID LINE ACTION                          
*                                                                               
*&&UK                                                                           
INFEOT   EQU   177                                                              
*&&                                                                             
*&&US                                                                           
INFEOT   EQU   194                                                              
*&&                                                                             
INFUPD   EQU   185                                                              
*                                                                               
ARZERO   DC    16F'0'                                                           
*                                                                               
DOTS     DC    40C'.'                                                           
EFFS     DC    16X'FF'                                                          
SPACES   DC    80C' '                                                           
HELPID   DC    XL10'0145FF00010000000000'                                       
DUMMY    BR    RE                                                               
*             ***....+....1....+....2....+....3....+....***                     
FLHEADER DC    C'S Root Sys/Prg Lng Adv  SE-sys  Ag Lv  '                       
         DC    C' S Root Sys/Prg Lng Adv  SE-sys  Ag Lv  '                      
FLUNDER  DC    C'- ---- ------- --- ---- ------- -- --- '                       
         DC    C' - ---- ------- --- ---- ------- -- --- '                      
*                                                                               
       ++INCLUDE FASYSLST                                                       
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
FULL1    DS    F                                                                
MYTIME   DS    F                   START TIME                                   
*                                                                               
IPARMS   DS    0XL32                                                            
ASYSFACS DS    A                   A(SYSTEM FACILITIES TABLE)                   
ATIA     DS    A                   A(TIA)                                       
AUTL     DS    A                   A(UTL ENTRY)                                 
ACOMFACS DS    A                   A(COMMON FACILITIES TABLE)                   
ASELIST  DS    A                   A(SELIST ENTRY)                              
ATWA     DS    A                   A(TWA)                                       
AMAP     DS    A                   A(PHASE MAP)                                 
ATIOB    DS    A                   A(TRANSLATOR INPUT OUTPUT BLOCK)             
*                                                                               
VHEXOUT  DS    A                                                                
VHEXIN   DS    A                                                                
VGETTXT  DS    A                                                                
VDATCON  DS    A                                                                
VGETHELP DS    A                                                                
VSCANNER DS    A                                                                
ACALLOV  DS    A                                                                
ALOCKSPC DS    A                                                                
ADMGR    DS    A                                                                
*                                                                               
ASAVE    DS    A                                                                
ALANG    DS    A                                                                
ALAST    DS    A                                                                
AVRSNMAX DS    A                                                                
BASERD   DS    A                                                                
RELO     DS    A                                                                
ALET     DS    A                                                                
SAVERE   DS    A                                                                
AHELP    DS    A                                                                
*                                                                               
DMCB     DS    6F                                                               
DSPHD    DS    XL64                                                             
*                                                                               
HALF     DS    H                                                                
BYTE     DS    C                                                                
DDS      DS    X                                                                
DDSNEW   EQU   X'80'               NEW SESSION                                  
DDSEOT   EQU   X'40'               END OF TABLE ENCOUNTERED                     
DDSUPD   EQU   X'20'               TABLE UPDATED                                
DDSDEL   EQU   X'10'               DELETE VIA OPERATOR (IN PARAM3)              
WORK     DS    CL80                                                             
ACTN     DS    X                                                                
*                                                                               
VIRGIN   DS    CL8                 VERSION TAB ENTRY                            
VSAVE    DS    CL8                 SAVED VERSION TAB ENTRY                      
TSPP1    DS    CL2                 SYS/PRG ARGUMENTS                            
TSPP2    DS    CL2                                                              
TSPP3    DS    CL2                                                              
PASS     DS    CL1                                                              
INPENTRY DS    CL8                 SPECIAL OPERATOR ENTRY (FROM P3)             
*                                                                               
FLSYS    DS    X                   SYSTEM FILTER                                
*                                                                               
GTSYS    DS    X                   OV SYSTEM                                    
GTSYSSE  DS    X                   SE SYSTEM                                    
GTSYSL   DS    X                   LEN FOR OVSYS NAME SEARCH                    
GTSYSN   DS    CL7                 SYS NAME                                     
GTPROGL  DS    X                   LEN FOR PROG NAME SEARCH                     
GTPROGN  DS    CL7                 PROG NAME                                    
GTPROG   DS    X                   PROG NUMBER                                  
GTLANGL  DS    X                                                                
GTLANGN  DS    CL3                 LANGUAGE NAME                                
GTLANG   DS    X                   LANGUAGE CODE                                
GTAPGMS  DS    A                   A(PRGMS LIST FOR SYSTEM)                     
GTADV    DS    X                   ADV NUMBER                                   
GTADVN   DS    CL4                 ADV 4CHR SYSTEM NAME                         
*                                                                               
PFKEY    DS    X                                                                
COUNT    DS    X                                                                
TRM      DS    H                                                                
RECLEN   DS    H                                                                
FACID    DS    CL4                                                              
FACNA    DS    CL3                                                              
PAGE     DS    X                                                                
*                                                                               
FADRH    DS    F                   ADDR OF ERROR FIELD HEADER                   
FERN     DS    H                   ERROR NUMBER (FOR MESSAGE)                   
HDRN     DS    H                   HEADER MESSAGE NUMBER (NO ERROR)             
FERRDSP  DS    X                   DISPLACEMENT TO ERROR IN FIELD               
*                                                                               
HELPKEY  DS    0CL10                                                            
         DS    XL3                                                              
HELPNUM  DS    XL1                                                              
HELPPAG  DS    XL1                                                              
         DS    XL5                                                              
*                                                                               
SLANG    DS    X                                                                
STEST    DS    X                                                                
STEST1   DS    X                                                                
*                                                                               
PSREC    DS    XL(PROGSPCL)                                                     
*                                                                               
SAVED    DS    0F                  SAVED INFORMATION                            
VERWORD  DS    CL8                 C'$VERSION'                                  
OFLSYS   DS    CL1                 OLD SYSTEM FILTER                            
SVPAGE   DS    CL1                                                              
SVTIME   DS    CL4                                                              
         DS    CL128                                                            
SAVEDLQ  EQU   *-SAVED                                                          
*                                                                               
IOAREA   DS    600D                4K WORK AREA                                 
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT COVERING DISPLAY DATA                                         *         
***********************************************************************         
         SPACE 1                                                                
VERLINED DSECT                                                                  
VLSELH   DS    CL8                 SELECT                                       
VLSEL    DS    CL1                                                              
VLROOTH  DS    CL8                 ROOT PHASE TSPP                              
VLROOT   DS    CL4                                                              
VLSYSPH  DS    CL8                 SYS/PROG                                     
VLSYSP   DS    CL7                                                              
VLLANGH  DS    CL8                 LANG                                         
VLLANG   DS    CL3                                                              
VLADVH   DS    CL8                 ADV                                          
VLADV    DS    CL4                                                              
VLSENUMH DS    CL8                 SE SYSTEM                                    
VLSENUM  DS    CL7                                                              
VLAGYH   DS    CL8                 AGENCY                                       
VLAGY    DS    CL2                                                              
VLLEVELH DS    CL8                 LEVEL                                        
VLLEVEL  DS    CL2                                                              
VLFLAGS  DS    CL2                 FLAGS                                        
VLLINEL  EQU   *-VERLINED                                                       
         EJECT                                                                  
***********************************************************************         
* SCREEN DSECT                                                        *         
***********************************************************************         
         SPACE 1                                                                
SRVERFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE SRVERFFD                                                       
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
*DDFLDHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*FADSECTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
*FASYSLSTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
*FAPROGSPCD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAPROGSPCD                                                     
         PRINT ON                                                               
*CTGENPHASE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENPHASE                                                     
         PRINT ON                                                               
*FACIDTABD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
*DMSPACED                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
*FATABSDEQU                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSDEQU                                                     
         PRINT ON                                                               
*DDFH                                                                           
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SRVER00   06/22/12'                                      
         END                                                                    
