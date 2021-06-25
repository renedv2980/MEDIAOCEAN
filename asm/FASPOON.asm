*          DATA SET FASPOON    AT LEVEL 043 AS OF 04/26/17                      
*&&      SET   CL=Y,AG=N                                                        
*PHASE T00A25A                                                                  
*INCLUDE GETBOOK                                                                
*INCLUDE REPLACE                                                                
*INCLUDE PQPROF                                                                 
***********************************************************************         
*PARAM1  XL1   X'FF'=RETURN A(REPORT KEY) IN PARAM6                   *         
*              X'00' RETURNED IF OK                                   *         
*              X'01' RETURNED IF JCL BOOK NOT FOUND                   *         
*              X'03' RETURNED IF JOB TABLE IS FULL                    *         
*        AL3   A(TWA)                                                 *         
*PARAM2  XL1   REQUEST CARD CONTROL VALUE                             *         
*        AL3   A(REQUEST CARD)                                        *         
*PARAM3  XL1   EXTENDED MULTIPLE REQUEST CARD NUMBER (PARAM2=X'FE')   *         
*        AL3   A(COMFACS)                                             *         
*PARAM4  XL1   N/D                                                    *         
*        AL3   A(JCLBOOK)                                             *         
*PARAM5  XL1   X'80'=SPOOK TRM OVERRIDES UTL, X'40'=P6 IS RFHDR                 
*        AL3   A(SPOOK)                                               *         
*PARAM6  XL1   N/D                                                    *         
*        AL3   A(REPORT KEY) RETURNED IF P1 BYTE 0 SET                *         
***********************************************************************         
         TITLE 'FASPOON - ON LINE JOB SPOOLER/SCHEDULER '                       
FASPOON  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 SPOONDL,**SPOON*,R7,R5,RR=RE,CLEAR=YES                           
         USING SPOOND,RC                                                        
         ST    RE,RELO                                                          
         LR    R9,R1                                                            
         L     RA,0(R9)            RA=A(TWA)                                    
         USING TWAD,RA                                                          
         SR    R8,R8                                                            
         ICM   R8,7,17(R9)                                                      
         USING SPOOK,R8            R8=A(SPOOK)                                  
*                                                                               
         BRAS  RE,INIT             PERFORM INITIALISATION                       
*                                                                               
* REQCON SELECTS ROUTINE                                                        
*                                                                               
         CLI   REQCON,X'FE'        TEST REQUEST CARD CONTROL BYTE               
         BE    EXTREQ              EXTENDED MULTIPLE REQUEST CARDS              
         CLI   REQCON,1                                                         
         BL    ALL                                                              
         BE    FIRST                                                            
         CLI   REQCON,3                                                         
         BE    ONLY                                                             
         CLI   REQCON,X'FF'                                                     
         BE    LAST                                                             
         CLI   REQCON,X'11'                                                     
         BL    SUBS                                                             
         CLI   REQCON,X'1F'                                                     
         BH    SUBS                                                             
*                                                                               
BIGREQ   BRAS  RE,BEFORE           X'1N'=BIG RQST CARD (N=#CRD ENTRIES)         
         BNE   EXIT                JOB TABLE IS FULL                            
         BRAS  RE,RESTRICT                                                      
         BRAS  RE,GETJCL                                                        
         BNE   EXIT                                                             
         BRAS  RE,RFHDR            OUTPUT REQ HDR CARD IF REQUIRED              
         ZIC   R0,REQCON           PICK UP NUMBER OF CARDS                      
         SLL   R0,32-4                                                          
         SRL   R0,32-4                                                          
         L     R1,AREQCRDS         POINT TO FIRST CARD IN RECORD                
BIGREQ2  MVC   CARD,0(R1)                                                       
         BRAS  RE,PUT                                                           
         LA    R1,80(,R1)                                                       
         BCT   R0,BIGREQ2                                                       
         BRAS  RE,LASTJCL                                                       
         BRAS  RE,AFTER                                                         
         B     EXIT                                                             
*                                                                               
EXTREQ   EQU   *                   EXTENDED MULTIPLE REQUEST CARDS              
         BRAS  RE,BEFORE           NUMBER OF CARDS IN PARAM3                    
         BNE   EXIT                JOB TABLE IS FULL                            
         BRAS  RE,RESTRICT                                                      
         BRAS  RE,GETJCL                                                        
         BNE   EXIT                                                             
         BRAS  RE,RFHDR            OUTPUT REQ HDR CARD IF REQUIRED              
         ZIC   R0,8(R9)            PICK UP NUMBER OF CARDS IN PARAM3            
         LTR   R0,R0                                                            
         BZ    EXTREQ4                                                          
         L     R1,AREQCRDS         POINT TO FIRST CARD IN RECORD                
EXTREQ2  MVC   CARD,0(R1)                                                       
         BRAS  RE,PUT                                                           
         LA    R1,80(,R1)                                                       
         BCT   R0,EXTREQ2                                                       
EXTREQ4  BRAS  RE,LASTJCL                                                       
         BRAS  RE,AFTER                                                         
         B     EXIT                                                             
*                                                                               
ALL      BRAS  RE,BEFORE           X'00'=NO CARDS                               
         BNE   EXIT                JOB TABLE IS FULL                            
         BRAS  RE,RESTRICT                                                      
         BRAS  RE,GETJCL                                                        
         BNE   EXIT                                                             
         BRAS  RE,AFTER                                                         
         B     EXIT                                                             
*                                                                               
FIRST    BRAS  RE,BEFORE           X'01'=FIRST CARD                             
         BNE   EXIT                JOB TABLE IS FULL                            
         BRAS  RE,RESTRICT                                                      
         BRAS  RE,GETJCL                                                        
         BNE   EXIT                                                             
         BRAS  RE,RFHDR            OUTPUT REQ HDR CARD IF REQUIRED              
*                                                                               
SUBS     MVC   CARD,REQ            X'02'=SUBSEQUENT CARDS                       
         BRAS  RE,PUT                                                           
         B     EXIT                                                             
*                                                                               
ONLY     BRAS  RE,BEFORE           X'03'=ONE DATE CARD ONLY                     
         BNE   EXIT                JOB TABLE IS FULL                            
         BRAS  RE,RESTRICT                                                      
         BRAS  RE,GETJCL                                                        
         BNE   EXIT                                                             
         BRAS  RE,RFHDR            OUTPUT REQ HDR CARD IF REQUIRED              
*                                                                               
LAST     MVC   CARD,REQ            X'FF'=LAST CARD                              
         BRAS  RE,PUT                                                           
         BRAS  RE,LASTJCL                                                       
         BRAS  RE,AFTER                                                         
*                                                                               
EXIT     ICM   R2,15,ASPSV         TEST IF SPOOL SAVE AREA DEFINED              
         BZ    EXIT010                                                          
         GOTO1 PROTOFF             DISABLE CALLER'S STORAGE PROTECTION          
         MVC   0(L'SPSV,R2),SPSV   RESTORE SAVE DATA                            
         GOTO1 PROTON              RESTORE CALLER'S STORAGE PROTECTION          
*                                                                               
EXIT010  LA    RF,JESKEY                                                        
         CLI   0(R9),X'FF'         TEST CALLER WANTS KEY RETURNED               
         BNE   *+8                                                              
         ST    RF,20(R9)           RETURN A(REPORT KEY) IN P6                   
         MVI   0(R9),0             RESET RETURN BYTE                            
         OC    JESKEY,JESKEY       WAS JCL BOOK FOUND?                          
         BNZ   *+12                YES                                          
         MVI   0(R9),1             NO  RETURN ERROR                             
         B     FREENTRY            FREE JOB ENTRY                               
*                                                                               
         CLI   SPOOKWEN,7                                                       
         BE    FREENTRY                                                         
*                                                                               
         CLI   RUNJOB,YES          RUN THIS JOB?                                
         BE    XIT                 YES                                          
*        CLC   =C'TSO',JESKDID     IF SUBID IS TSO. . .                         
*        BNE   XIT                                                              
*        L     RF,AUTL             . . . AND IT'S A DDS TERMINAL. . .           
*        TM    TSTAT1-UTLD(RF),TSTATDDS                                         
*        BZ    XIT                 . . . THEN IT'S NOT SUBMITTED                
         B     FREENTRY                                                         
         EJECT                                                                  
***********************************************************************         
* FREE ENTRY IN JOB TABLE                                             *         
***********************************************************************         
FREENTRY GOTO1 PROTOFF             TURN OFF FACPAK PROTECTION                   
*                                                                               
         L     RF,ASSB                                                          
         ICM   RF,15,SSBTKADR-SSBD(RF)                                          
         ICM   R2,15,TCBSJENT-TCBD(RF)                                          
         BZ    FREEX                                                            
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,TBLET                                                    
         SAC   512                                                              
         USING TBJNTRY,R2                                                       
         MVI   TBJSTAT,JOBSAVA                                                  
         BRAS  RE,ARSOFF                                                        
*                                                                               
FREEX    GOTO1 PROTON                                                           
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIAL - ID DETAILS                                                *         
***********************************************************************         
BEFORE   NTR1  ,                                                                
         CLI   SPOOKWEN,7          SPECIAL OVERNIGHT SOON                       
         BE    BEFR02                                                           
*                                                                               
         BRAS  RE,FINDNTRY         YES GRAB AN EMPTY JOB TABLE ENTRY            
         L     RF,ASSB                                                          
         L     RF,SSBTKADR-SSBD(RF)                                             
         ICM   RE,15,TCBSJENT-TCBD(RF)                                          
         BNZ   BEFR02                                                           
         DC    H'0',C'$JTFULL '    TABLE IS FULL - UNWIND WITH ERROR            
*                                                                               
BEFR02   XC    KEY,KEY             GET ID RECORD                                
         LA    R4,KEY                                                           
         USING CTIREC,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),SPOOKUID                                             
         BRAS  RE,READ                                                          
*                                                                               
         MVC   ORIGNADD,SPACES     ORIGIN DETAILS                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'36'                                                     
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   ORIGNADD,2(R6)                                                   
*                                                                               
         OC    SPOOKDES,SPOOKDES   DESTINATION DETAILS                          
         BZ    BEFR04                                                           
         CLC   SPOOKDES,SPOOKUID                                                
         BE    BEFR04                                                           
*                                                                               
         XC    KEY,KEY             GET DEST ID RECORD                           
         LA    R4,KEY                                                           
         USING CTIREC,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),SPOOKDES                                             
         BRAS  RE,READ                                                          
*                                                                               
BEFR04   LA    R6,IO                                                            
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTDSTD,R6                                                        
         MVC   LOGOS,CTDSTLG1                                                   
         MVC   JESCODE,CTDSTPOW                                                 
         NC    SPOOKDID,SPOOKDID                                                
         BNZ   BEFR06                                                           
         MVC   SPOOKDID,TWADIRCT                                                
*                                                                               
*&&UK*&& CLC   SPOOKSYS,=C'ME'                                                  
*&&UK*&& BNE   *+10                                                             
*&&UK*&& MVC   SPOOKDID,JESJOB+4                                                
*                                                                               
BEFR06   MVI   JOBCLA,C'A'         SET DEFAULT JOB CLASS                        
         MVI   JOBPRI,C'5'         SET DEFAULT JOB PRIORITY                     
*                                                                               
*&&US*&& MVI   PRTCLA,C'A'         SET DEFAULT SYSLST CLASS                     
*&&US*&& MVC   PRTFNO,=C'1S  '     SET DEFAULT SYSLST FORMS                     
*&&UK*&& MVI   PRTCLA,C'F'                                                      
*&&UK*&& MVC   PRTFNO,=C'1SML'                                                  
*&&UK*&& MVI   PRTQ,C'Y'           SET WE WANT PRTQUE DD STATEMENTS             
*                                                                               
         MVC   JESJOB(4),JESCODE                                                
         MVC   JESJOB+4(1),SPOOKSYS                                             
         CLC   SPOOKSYS,=C'MP'                                                  
         BNE   *+8                                                              
         MVI   JESJOB+4,C'L'                                                    
         CLC   SPOOKSYS,=C'MB'                                                  
         BNE   *+8                                                              
         MVI   JESJOB+4,C'B'                                                    
         MVC   JESJOB+5(2),BOOK+3                                               
         MVI   JESJOB+7,C'S'       S=SOON                                       
         CLI   JESJOB+3,C' '       CATER FOR 3 CHR AGY ID                       
         BNE   BEFR08                                                           
         MVI   JESJOB+3,C'X'                                                    
         CLI   JESJOB+2,C' '       ALSO FOR 2 CHR                               
         BNE   BEFR08                                                           
         MVI   JESJOB+2,C'X'                                                    
*                                                                               
BEFR08   MVC   JESCARD,SPACES                                                   
         CLI   SPOOKWEN,8                                                       
         BNE   BEFR10                                                           
         MVC   CARD,=CL80'MQ EXTERNAL JOB REQUEST PENDING'                      
         BRAS  RE,PUT              WRITE DUMMY JOB CARD                         
         B     BEFR16                                                           
*                                                                               
BEFR10   MVC   CARD,MVSJOB         SET FIRST MVS JOB CARD                       
         MVC   CARD+2(8),JESJOB                                                 
         LA    RE,CARD+15          POINT TO ACC INFO FIELD                      
         MVC   0(4,RE),JESJOB                                                   
         MVC   4(1,RE),SPOOKSYS                                                 
         CLC   SPOOKSYS,=C'MP'                                                  
         BNE   *+8                                                              
         MVI   4(RE),C'L'                                                       
         CLC   SPOOKSYS,=C'MB'                                                  
         BNE   *+8                                                              
         MVI   4(RE),C'B'                                                       
         MVI   5(RE),C'0'                                                       
         MVC   6(2,RE),BOOK+3                                                   
         MVI   8(RE),C'1'                                                       
*&&UK                                                                           
         CLC   SPOOKSYS,=C'ME'                                                  
         BNE   BEFR14                                                           
         LA    R1,MESPTAB          SEARCH MEDIA SYS SPECIALS                    
         USING SPTABD,R1                                                        
*                                                                               
BEFR12   CLI   0(R1),EOTQ                                                       
         BE    BEFR14                                                           
         CLC   SPPROG,6(RE)        TEST IF PROGRAM PP IN LIST                   
         BE    *+12                                                             
         LA    R1,SPTABLQ(R1)                                                   
         B     BEFR12                                                           
         MVC   5(1,RE),SPSUBSYS    SET SUBSYSTEM CHARACTER                      
         B     BEFR14                                                           
*                                                                               
MESPTAB  DC    C'CR1CT1FC1FG1FQ1FT1NC1NG1NT1YQ1' CRAFT REQUESTS                 
         DC    AL1(EOTQ)                                                        
         DROP  R1                                                               
*&&                                                                             
BEFR14   BRAS  RE,PUT              WRITE FIRST JOB CARD                         
*                                                                               
BEFR16   MVC   CARD,MVSJOB1        SET SECOND MVS JOB CARD                      
*&&US*&& MVI   CARD+24,C'0'        MSGCLASS=0                                   
         OC    SPOOKPR1(2),SPOOKPR1                                             
         BNZ   BEFR22              CALLER PASSED SPOOK WITH JOB INFO            
*                                                                               
*&&UK*&& CLC   SPOOKSYS,=C'ME'     SET MEDIA SYSTEM DEFAULTS                    
*&&UK*&& BNE   BEFR18                                                           
*&&UK*&& MVI   JOBCLA,C'M'         MEDIA SYSTEM CLASS/PRTY                      
*&&UK*&& MVI   JOBPRI,C'5'                                                      
*&&UK*&& B     BEFR26                                                           
*                                                                               
BEFR18   CLI   SPOOKSYS,C'A'       SET ACCOUNT SYSTEM DEFAULTS                  
         BNE   BEFR20                                                           
         MVI   JOBCLA,C'A'         ACCOUNT SYSTEM CLASS/PRTY                    
         MVI   JOBPRI,C'5'                                                      
         CLI   REQCON,3            AS LONG AS IT'S A 1 CARD REQUEST             
         BE    BEFR26                                                           
         CLI   REQCON,X'11'        AS ABOVE                                     
         BE    BEFR26                                                           
*&&UK*&& MVI   JOBPRI,C'5'                                                      
*&&US*&& MVI   JOBPRI,C'5'                                                      
         B     BEFR26                                                           
*                                                                               
BEFR20   B     BEFR26                                                           
*                                                                               
BEFR22   CLI   SPOOKPR1,C'A'       TEST VALID SPOOK CLASS                       
         BL    BEFR24                                                           
         CLI   SPOOKPR1,C'9'                                                    
         BH    BEFR24                                                           
         MVC   JOBCLA,SPOOKPR1     SET JOB CLASS FROM SPOOK                     
*                                                                               
BEFR24   CLI   SPOOKPR2,C'0'       TEST VALID SPOOK PRIORITY                    
         BL    BEFR26                                                           
         CLI   SPOOKPR2,C'9'                                                    
         BH    BEFR26                                                           
         MVC   JOBPRI,SPOOKPR2     SET JOB PRIORITY FROM SPOOK                  
*                                                                               
BEFR26   CLI   SPOOKWEN,8          TEST MQIO EXTERNAL JOB REQUEST               
         BE    BEFR44              IF SO DONT PUT JOB CARDS                     
         MVC   CARD+32(1),JOBCLA   SET CLASS/PRTY AND RELEASE                   
         MVC   CARD+39(1),JOBPRI                                                
*&&UK                                                                           
         B     BEFR30        <---  REMOVE THIS TO RUN MONSOON A.M.P             
         CLI   JOBCLA,C'A'         TYPRUN=SCAN FOR CLASS A.M.P                  
         BE    BEFR28                                                           
         CLI   JOBCLA,C'M'                                                      
         BE    BEFR28                                                           
         CLI   JOBCLA,C'P'                                                      
         BE    BEFR28                                                           
         B     BEFR30                                                           
*                                                                               
BEFR28   CLC   SPOOKUID,=X'007C'   ID=JWAL                                      
         BNE   *+14                                                             
         CLC   SPOOKDID,=C'DDS'                                                 
         BE    *+10                                                             
         MVC   CARD+40(12),=C',TYPRUN=SCAN'                                     
*&&                                                                             
BEFR30   BRAS  RE,PUT                                                           
*                                                                               
BEFR32   MVC   MAINCARD,SPACES     INITIALISE DEFAULT JES MAIN CARD             
         BRAS  RE,SETJTCLS         SET MAINCLAS                                 
*&&UK*&& B     BEFR44                                                           
*&&US                                                                           
BEFR34   CLI   SPOOKSYS,C'A'       ACCOUNT                                      
         BE    BEFR40                                                           
         CLI   SPOOKSYS,C'N'       NETWORK                                      
         BE    BEFR40                                                           
         CLI   SPOOKSYS,C'S'       SPOT                                         
         BE    BEFR40                                                           
         CLI   SPOOKSYS,C'P'       PRINT                                        
         BE    BEFR40                                                           
         CLI   SPOOKSYS,C'R'       REP                                          
         BE    BEFR40                                                           
         CLI   SPOOKSYS,C'B'       MEDIA BASE                                   
         BE    BEFR40                                                           
         CLI   SPOOKSYS,C'C'       CONTROL                                      
         BE    BEFR40                                                           
         CLI   SPOOKSYS,C'D'       CPP                                          
         BE    BEFR40                                                           
         CLI   SPOOKSYS,C'T'       TALENT                                       
         BE    BEFR40                                                           
         CLI   SPOOKSYS,C'M'       MEDIA PLANNING                               
         BNE   BEFR44                                                           
*                                                                               
BEFR40   MVC   MAINCARD,JESMAIN    //*MAIN CLASS=....                           
         MVC   MAINCARD+14(3),MAINCLAS                                          
         MVC   MAINCARD+17(4),=C'SOON'                                          
         B     BEFR42                                                           
*&&                                                                             
BEFR42   MVC   CARD,MAINCARD                                                    
         BRAS  RE,PUT                                                           
*                                                                               
BEFR44   BRAS  RE,SETSOFT                                                       
         CLI   SPOOKWEN,7          SPECIAL OVERNIGHT SOON                       
         BE    BEFR48                                                           
*                                                                               
         GOTO1 PROTOFF                                                          
         BRAS  RE,ARSOFF                                                        
*                                                                               
         L     RF,ASSB                                                          
         L     RF,SSBTKADR-SSBD(RF) A(TCB ENTRY)                                
         L     R2,TCBSJENT-TCBD(RF) A(THIS JOB TABLE ENTRY)                     
         LAM   AR2,AR2,TBLET                                                    
         SAC   512                                                              
         USING TBJOBTAB,R2                                                      
         MVC   TBJMVSID,JESJOB     SAVE MVS JOBNAME                             
         DROP  R2                                                               
*                                                                               
BEFR46   BRAS  RE,ARSOFF                                                        
         GOTO1 PROTON                                                           
*                                                                               
BEFR48   CR    RE,RE               SET CC EQUAL                                 
*                                                                               
BEFXIT   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* AFTER                                                               *         
***********************************************************************         
AFTER    NTR1  ,                                                                
         CLI   MOREJCL,C'Y'                                                     
         BNE   *+8                                                              
         BRAS  RE,GETJCL                                                        
         GOTOR GOPOWWOW,DMCB,=C'SCH',=C'EOJ',JESKEY,JESCARD                     
         CLI   SPOOKWEN,7          SPECIAL OVERNIGHT SOON                       
         BE    XIT                                                              
         BRAS  RE,FILLNTRY         YES FILL IN JOB TABLE ENTRY                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CREATE RFHDR= CARD THAT PRECEEDS EACH REQUEST            *         
***********************************************************************         
RFHDR    NTR1  ,                                                                
         LA    R4,RFHDATA          R4=A(REQ FILE HEADER DATA)                   
         USING RQHHDR,R4                                                        
         XC    RQHHDR,RQHHDR                                                    
         MVC   RQHSYSID,SYSID      FACPAK SYSTEM ID                             
*                                                                               
         TM    16(R9),X'40'        TEST USER PASSED A(RFHDR) IN P6              
         BZ    RFHDR1                                                           
         L     RE,20(R9)                                                        
         MVC   RQHITRM,0(RE)       THEN JUST MOVE IT                            
         B     RFHDR2                                                           
*                                                                               
         USING UTLD,R3                                                          
RFHDR1   L     R3,AUTL             R3=A(UTL ENTRY)                              
         OI    RQHFLAG1,RQHFNEW    SET NEW STYLE TERMINAL INFO                  
         MVC   RQHSIN,TSIN+1                                                    
         OI    RQHINFO,RQHIONL     SET REQUEST ADDED ONLINE                     
         TM    TSTAT1,TSTATDDS                                                  
         BZ    *+8                                                              
         OI    RQHINFO,RQHIDDS     SET REQUEST ADDED BY DDS TERMINAL            
         TM    TFLAG,TFLAGSEC                                                   
         BZ    *+8                                                              
         OI    RQHINFO1,RQHISEC    SET USERID REQUIRES A PASSWORD               
         TM    TSTATB,TSTATPPS                                                  
         BZ    *+8                                                              
         OI    RQHINFO1,RQHIPPS    SET AGENCY USES PPS SECURITY                 
         MVC   RQHSAGN,TSAGN                                                    
         MVC   RQHSAGYP,TAGYPER    SET PERSON SECURITY AGY                      
         OC    RQHSAGYP,RQHSAGYP                                                
         BNZ   *+10                                                             
         MVC   RQHSAGYP,TAGYSEC                                                 
         MVC   RQHSAGY,TAGYSEC     SET AGENCY SECURITY AGENCY                   
*                                                                               
         MVC   RQHACCS,TACCS                                                    
         MVC   RQHPSWD,TPASSWD                                                  
         MVC   RQHSYS,TSYS                                                      
         MVC   RQHPRG,TPRG                                                      
         MVC   RQHCTRY,TCTRY                                                    
         MVC   RQHLANG,TLANG                                                    
         MVC   RQHAGCTY,TAGCTRY                                                 
*                                                                               
         MVC   RQHOFF,TOFFCODE     OFFICE NUMBER                                
         MVI   RQHCTRL,0                                                        
         CLI   SPOOKQTY,SPOOKQCS   TEST COMSCORE                                
         BNE   *+8                                                              
         MVI   RQHCTRL,1           YES-SET COMSCORE FLAG IN HDR                 
         MVC   RQHAGY,TAGY         AGENCY ALPHA                                 
         MVC   RQHORIG,TUSER       MOVE ORIGINATING USER ID NUM                 
*                                                                               
RFHDR2   MVC   RFHCARD,SPACES      BUILD RFHDR= CARD                            
         MVC   RFHCARD(6),=C'RFHDR='                                            
         MVC   RFHCARD+6(20),RQHITRM                                            
         MVI   RFHCARD+6+16,0                                                   
         CLI   SPOOKQTY,SPOOKQCS   IF COMSCORE SET VALUE IN RFHDR=CARD          
         BNE   *+8                                                              
         MVI   RFHCARD+6+16,1                                                   
*                                                                               
RFHDR3   CLC   SPOOKXT(3),=C'XT='  TEST IF EXTENDED SPOOK                       
         BNE   RFHDR4                                                           
         LA    RF,RFHCARD+27                                                    
         OC    SPOOKPSW,SPOOKPSW   TEST IF PID/PIN VALUE DEFINED                
         BZ    RFHDR4                                                           
         TM    SPOOKSF1,X'40'      TEST NONO FLAG                               
         BO    RFHDR4                                                           
         TM    SPOOKSF1,X'10'      TEST PIN                                     
         BZ    *+12                                                             
         MVI   0(RF),2                                                          
         B     RFHDR3A                                                          
         TM    SPOOKSF1,X'20'      TEST PID                                     
         BZ    *+12                                                             
         MVI   0(RF),3                                                          
         B     RFHDR3A                                                          
         B     RFHDR4                                                           
RFHDR3A  MVC   1(4,RF),SPOOKPSW    SET PID/PIN                                  
         MVI   5(RF),0                                                          
*                                                                               
RFHDR4   MVC   CARD,RFHCARD        RELEASE CARD                                 
RFHDR5   BRAS  RE,PUT                                                           
*                                                                               
RFHDRX   B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET SOFT VALUES                                          *         
***********************************************************************         
SETSOFT  NTR1                                                                   
         XC    SOFTNTRY,SOFTNTRY                                                
         L     R2,ASOFTABL                                                      
         A     R2,RELO                                                          
         LA    R3,SOFTWORK                                                      
SETSOFT2 CLI   0(R2),0                                                          
         BE    XIT                                                              
         MVC   0(02,R3),0(R2)                                                   
         MVC   2(15,R3),5(R2)                                                   
         SR    RF,RF                                                            
         ICM   RF,7,2(R2)                                                       
         A     RF,RELO                                                          
         MVC   WORK(15),SPACES                                                  
         BASR  RE,RF                                                            
         MVC   17(15,R3),WORK                                                   
         L     R1,SOFTNTRY                                                      
         LA    R1,1(R1)                                                         
         ST    R1,SOFTNTRY                                                      
         LA    R2,L'SOFTABLE(R2)                                                
         LA    R3,L'SOFTWORK(R3)                                                
         B     SETSOFT2                                                         
*                                                                               
SETAGY   MVC   WORK(2),SPOOKAGY                                                 
         BR    RE                                                               
SETCMP   MVC   WORK(1),SPOOKAGY                                                 
         BR    RE                                                               
SETLDG   MVC   WORK(1),SPOOKAGY+1                                               
         BR    RE                                                               
SETREP   MVC   WORK(2),SPOOKAGY                                                 
         BR    RE                                                               
SETDAT   MVC   WORK(8),TODAY                                                    
         BR    RE                                                               
SETBIL   MVC   WORK(8),BILLDATE                                                 
         BR    RE                                                               
SETPRG   MVC   WORK(2),SPOOKEOD                                                 
         BR    RE                                                               
SETSYS   MVC   WORK(2),SPOOKSYS                                                 
         BR    RE                                                               
SETLG1   MVC   WORK(7),LOGOS                                                    
         BR    RE                                                               
SETLG2   MVC   WORK(7),LOGOS+7                                                  
         BR    RE                                                               
SETFAC   MVC   WORK(1),FACID                                                    
         BR    RE                                                               
SETORG   SR    R0,R0                                                            
         ICM   R0,3,SPOOKUID                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(6),DUB                                                      
         BR    RE                                                               
SETDAY   DS    0H                                                               
*&&UK*&& MVC   WORK(2),TODAY                                                    
*&&US*&& MVC   WORK(2),TODAY+3                                                  
         BR    RE                                                               
SETHEX   LR    R0,RE                                                            
         GOTO1 HEXOUT,DMCB,SPOOKAGY,WORK,1                                      
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* HANDLE JCL FROM CONTROL FILE. EXIT WITH CC=NEQ IF JCL BOOK NOT FOUND*         
***********************************************************************         
GETJCL   NTR1  ,                                                                
         CLI   SPOOKWEN,8          TEST MQIO EXTERNAL JOB REQUEST               
         BE    GJY                 IF SO EXIT WITH NO JCL                       
         CLI   MOREJCL,C'Y'                                                     
         BE    GJ2                                                              
         MVI   IF,C'Y'                                                          
         LA    R4,BOOKBUFF         SET UP BOOK KEY                              
         XCEF  BOOKBUFF,'2*K'                                                   
         USING CTJREC,R4                                                        
         XC    CTJKEY,CTJKEY                                                    
         MVI   CTJKTYP,C'J'                                                     
         MVC   CTJKID,BOOK                                                      
         MVI   CTJKID+2,C'V'       SET MVS BOOK ID                              
*                                                                               
         GOTO1 GETBOOK,DMCB,BOOKBUFF,CARD,DATAMGR                               
         CLI   8(R1),0                                                          
         BE    GJ3                                                              
         TM    8(R1),X'12'         TEST NOT FOUND                               
         JZ    *+2                 YES - ERASE CARDS & KEY                      
*                                                                               
         GOTOR GOPOWWOW,DMCB,=C'SCH',=C'END',0,0                                
         XC    JESKEY,JESKEY                                                    
         B     GJN                                                              
*                                                                               
GJ2      GOTO1 GETBOOK,DMCB,BOOKBUFF,CARD,DATAMGR                               
         TM    8(R1),X'80'                                                      
         BO    GJ6                                                              
         CLI   8(R1),0                                                          
         BE    GJ3                                                              
         TM    8(R1),X'12'         TEST NOT FOUND                               
         JZ    *+2                 YES - ERASE CARDS & KEY                      
*                                                                               
         GOTOR GOPOWWOW,DMCB,=C'SCH',=C'END',0,0                                
         XC    JESKEY,JESKEY                                                    
         B     GJN                                                              
GJ3      CLC   CARD(2),=C'$$'      TEST DDS JCL STATEMENT                       
         BNE   *+10                                                             
         MVC   CARD(2),MVSJOB      YES - CONVERT TO MVS                         
         CLC   CARD(13),=C'REQUESTS HERE'                                       
         BE    GJ10                                                             
         CLC   CARD(9),=C'LOAD=AC21'  TEST OLD ACC BILLING                      
         BE    GJ30                                                             
         CLC   CARD(12),=C'CONTROL CARDS'                                       
         BE    GJY                                                              
         CLC   CARD(10),=C'SOON PRTQ='                                          
         BNE   *+14                                                             
         MVC   PRTQ,CARD+10                                                     
         B     GJ2                                                              
*                                                                               
         CLC   CARD(6),=C'IF END'  IF CONDITIONS                                
         BNE   *+12                                                             
         MVI   IF,C'Y'                                                          
         B     GJ2                                                              
*                                                                               
         CLC   CARD(2),=C'IF'                                                   
         BNE   *+12                                                             
         MVI   IF,C'N'                                                          
         B     IFTEST                                                           
*                                                                               
         CLC   CARD(3),=C'OR '                                                  
         BE    IFTEST                                                           
         CLC   CARD(3),=C'AND'                                                  
         BNE   GJ4                                                              
         CLI   IF,C'N'                                                          
         BE    GJ2                                                              
         MVC   CARD(79),CARD+1     CHANGE AND TO ND                             
         MVI   IF,C'N'                                                          
         B     IFTEST                                                           
*                                                                               
GJ4      CLI   IF,C'N'                                                          
         BE    GJ2                                                              
         MVC   CARD+72(7),SPACES   DROP SEQ NO                                  
         MVI   EXEC,C'N'                                                        
         GOTO1 REPLACE,DMCB,(80,CARD),SOFTNTRY                                  
         BRAS  RE,PUT                                                           
         B     GJ2                                                              
*                                                                               
GJ6      XC    BOOKBUFF(25),BOOKBUFF                                            
         B     GJY                                                              
         EJECT                                                                  
***********************************************************************         
* EXTRA CONTROLLER CARDS                                                        
***********************************************************************         
GJ10     CLC   CARD+14(8),=C'AND MORE'                                          
         BNE   *+8                                                              
         MVI   MOREJCL,C'Y'                                                     
         MVC   CARD,SPACES                                                      
         MVI   CARD+2,X'F0'                                                     
         MVC   CARD+3(2),SPOOKEOD                                               
         MVC   CARD+5(2),SPOOKAGY                                               
         MVC   CARD(2),=C'MP'      PLANNING/BUDGET                              
         CLC   SPOOKSYS,=C'MP'                                                  
         BE    GJ12                                                             
         MVC   CARD(2),=C'MB'      MEDIABASE                                    
         CLC   SPOOKSYS,=C'MB'                                                  
         BE    GJ12                                                             
*&&UK                                                                           
         MVC   CARD(2),=C'FE'      ARTISTE FEES                                 
         CLC   SPOOKSYS,=C'FE'                                                  
         BE    GJ12                                                             
*&&                                                                             
         MVC   CARD(2),=C'PE'      PERSON                                       
         CLC   SPOOKSYS,=C'PE'                                                  
         BE    GJ11                                                             
*&&US                                                                           
         MVC   CARD(2),=C'ST'      SPOT TRAFFIC                                 
         CLC   SPOOKSYS,=C'ST'                                                  
         BE    GJ12                                                             
         MVC   CARD(2),=C'SP'      SPOT                                         
         CLI   SPOOKSYS,C'S'                                                    
         BE    GJ12                                                             
         MVC   CARD(2),=C'NE'      NETWORK GETS ITS OWN FOR NOW                 
         CLI   SPOOKSYS,C'N'                                                    
         BE    GJ12                                                             
         MVC   CARD(2),=C'PP'      PRINT                                        
         CLI   SPOOKSYS,C'P'                                                    
         BE    GJ12                                                             
         MVC   CARD(2),=C'RE'      REP                                          
         CLI   SPOOKSYS,C'R'                                                    
         BE    GJ12                                                             
         MVC   CARD(2),=C'TA'      TALENT                                       
         CLI   SPOOKSYS,C'T'                                                    
         BE    GJ12                                                             
*&&                                                                             
         MVC   CARD(2),=C'CT'      CONTROL                                      
         CLI   SPOOKSYS,C'C'                                                    
         BNE   *+14                                                             
         MVC   CARD+2(10),CARD+3                                                
         B     GJ12                                                             
         MVC   CARD(2),=C'AC'      ACCPAK                                       
         B     *+10                *NOP*                                        
GJ11     MVC   CARD+5(2),SPACES                                                 
         CLI   SPOOKSYS,C'A'                                                    
         BE    GJ12                                                             
*&&UK                                                                           
         MVC   CARD+2(10),CARD+3   MEDLINE                                      
         CLC   SPOOKSYS,=C'ME'                                                  
         BNE   GJ12                                                             
         MVC   CARD(2),=C'ME'                                                   
*&&                                                                             
GJ12     BRAS  RE,PUT                                                           
*                                                                               
         MVC   CARD(5),=C'LOGO='                                                
         MVC   CARD+5(14),LOGOS                                                 
         MVC   CARD+19(8),JESJOB                                                
         MVC   CARD+33(7),=C'ORIGIN='                                           
         SR    R0,R0                                                            
         ICM   R0,3,ID                                                          
*&&US*&& TM    16(R9),X'80'        TEST SPOOK PASSED BY USER                    
*&&UK*&& TM    16(R9),X'40'        TEST SPOOK PASSED BY USER                    
         BZ    *+8                                                              
         ICM   R0,3,SPOOKUID       IF YES, USE THAT USERID!                     
         EDIT  ((R0)),(5,CARD+40),FILL=0                                        
*                                                                               
         L     RF,AUTL                                                          
         OC    OVRDUTL(2),OVRDUTL     TEST FOR OVERRIDING UTL ENTRY             
         BZ    *+8                                                              
         LA    RF,OVRDUTL                                                       
*                                                                               
         MVC   CARD+50(3),=CL3'AG='                                             
         MVC   CARD+53(2),TAGY-UTLD(RF)                                         
         MVC   CARD+56(3),=CL3'LU='                                             
         MVC   CARD+59(8),TLUID-UTLD(RF)                                        
*&&US                                                                           
         MVC   CARD+68(3),=CL3'QC='                                             
         LA    R1,SOONLST          SET FROM SPECIAL LIST                        
GJ13     CLI   10(R1),C'?'                                                      
         BE    GJ13X                                                            
         CLC   SPOOKSYS(1),10(R1)                                               
         BE    GJ13X                                                            
         AHI   R1,L'SOONLST                                                     
         LA    R0,SOONLSTX                                                      
         CR    R1,R0                                                            
         BNH   GJ13                                                             
         DC    H'0'                                                             
*                                                                               
GJ13X    MVC   CARD+71(2),8(R1)    SET CLASS                                    
*                                                                               
         MVC   CARD+74(3),=CL3'TY='                                             
         MVI   CARD+77,C'R'                                                     
         CLC   SPOOKXT(3),=C'XT='  TEST IF EXTENDED SPOOK                       
         BNE   GJ13Z                                                            
         CLI   SPOOKQTY,SPOOKQLG   TYPE LONG?                                   
         BNE   *+8                 NO                                           
         MVI   CARD+77,SPOOKQLG    YES                                          
         *                                                                      
         CLI   SPOOKQTY,SPOOKQCS   TYPE COMSCORE?                               
         BNE   *+8                 NO                                           
         MVI   CARD+77,SPOOKQCS    YES                                          
*&&                                                                             
GJ13Z    BRAS  RE,PUT                                                           
*                                                                               
GJ14     L     RF,AUTL             CREATE LOGO2=TNUM FOR SPOT TRAFFIC           
         SR    R0,R0                                                            
         ICM   R0,3,TNUM-UTLD(RF)  GET TERMINAL NUMBER                          
         BZ    GJ14A                                                            
         CLI   TOVSYS-UTLD(RF),X'0D' TEST TRAFFIC SYSTEM                        
         BNE   GJ14A                                                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   CARD(6),=C'LOGO2='                                               
         UNPK  CARD+6(5),DUB                                                    
         BRAS  RE,PUT                                                           
*                                                                               
GJ14A    CLI   SPOOKSYS,C'A'       CREATE DMPARM=EMU CARD FOR ACC               
         BNE   GJ14B                                                            
         GOTO1 DATAMGR,DMCB,=C'DTFADD',=C'ACCOUNT'                              
         L     RE,12(R1)           GET A(ACCOUNT DCB)                           
         TM    ISFTYPE-ISDTF(RE),ISFTEMU                                        
         BZ    GJ14B               ACCFILE IS NOT EMULATED                      
         MVC   CARD(10),=C'DMPARM=EMU'                                          
         BRAS  RE,PUT                                                           
*        B     GJ15                                                             
*                                                                               
GJ14B    CLC   ORIGNADD,SPACES                                                  
         BE    GJ14C                                                            
         MVC   CARD(7),=C'ORIGIN='                                              
         MVC   CARD+7(66),ORIGNADD                                              
         BRAS  RE,PUT                                                           
*                                                                               
GJ14C    CLI   WRITENO,YES                                                      
         BNE   GJ14D                                                            
         MVC   CARD(8),=C'WRITE=NO'                                             
         BRAS  RE,PUT                                                           
*                                                                               
         USING UTLD,RF                                                          
GJ14D    L     RF,AUTL                                                          
         CLC   TTICKET,SPACES                                                   
         BNH   GJ15                                                             
         MVC   CARD(7),=C'TICKET='                                              
         MVC   CARD+7(8),TTICKET                                                
         BRAS  RE,PUT                                                           
         DROP  RF                                                               
*                                                                               
GJ15     MVC   CARD(10),=C'INPUT=CARD'                                          
         BRAS  RE,PUT                                                           
         MVC   CARD(7),=C'FACPAK='                                              
         MVC   CARD+7(1),FACID     CAME FROM SSB ORIGINALLY                     
         BRAS  RE,PUT                                                           
*&&UK                                                                           
         TM    FACFLG,SSBSYTST     IS THIS A TEST SYSTEM                        
         BNO   GJ16                                                             
         CLI   SPOOKSYS,C'M'                                                    
         BNE   GJ16                                                             
         MVC   CARD(8),=C'MEDDSPC='                                             
         MVC   CARD+8(3),FACNAM    SET MEDIA DATASPACE NAME IN JOB              
         BRAS  RE,PUT                                                           
*&&                                                                             
         USING UTLD,RF                                                          
GJ16     L     RF,AUTL             TEST IF BILLING INITIATED                    
         TM    TSTAT1,TSTATBIL                                                  
         BZ    GJ16X                                                            
                                                                                
         USING SSBD,RF                                                          
         L     RF,ASSB                                                          
         L     RF,SSBTKADR                                                      
*                                                                               
         USING TCBD,RF                                                          
         LA    RF,TCBBILL          RF=A(BILLING REF IN TCB)                     
         CLC   0(L'TCBBILL,RF),NULLS                                            
         BE    GJ16X                                                            
         MVC   CARD(8),=C'BILLREF='                                             
         GOTO1 HEXOUT,DMCB,(RF),CARD+8,12,=C'TOG'                               
         BRAS  RE,PUT                                                           
         DROP  RF                                                               
GJ16X    EQU   *                                                                
*                                                                               
GJ17     XC    MYREMOTE,MYREMOTE   CLEAR REMOTE PQ PROFILE AREA                 
         MVI   OKREMOTE,0                                                       
         XC    PROFKEY,PROFKEY     SET PARAM LIST FOR PQPROF                    
         MVC   PROFKEY(1),BOOK     PROFILE RECORD KEY SYSTEM/PROGRAM            
         MVC   PROFKEY+1(2),BOOK+3                                              
         OC    SPOOKDES,SPOOKDES   PICK UP USER ID FOR KEY                      
         BZ    *+14                                                             
         MVC   PROFKEY+3(2),SPOOKDES                                            
         B     *+10                                                             
         MVC   PROFKEY+3(2),SPOOKUID                                            
         L     RF,8(R9)            A(COMFACS)                                   
         OC    PQPROF,PQPROF       CHECK A(ROUTINE) IN COMFACS                  
         BZ    GJ17X                                                            
         GOTO1 PQPROF,DMCB,(X'80',PROFKEY),(0,MYREMOTE),(RF)                    
         CLI   8(R1),0                                                          
         BNE   GJ17X                                                            
         MVI   OKREMOTE,X'01'      PROFILE INFO IS DEFINED                      
GJ17X    EQU   *                                                                
*                                                                               
                                                                                
GJ18     MVI   REPORTYP,0          SET REPORT TYPE AND ARCHIVE DATA             
         MVI   ARCCLASS,0                                                       
         MVI   ARCTY1,0                                                         
         XC    ARCDOCTY,ARCDOCTY                                                
         CLC   SPOOKXT(3),=C'XT='  TEST IF EXTENDED SPOOK                       
         BNE   GJ18A                                                            
         NI    SPOOKTY,255-REMOTARQ   TURN OFF ARCHIVABLE REPORT                
         MVC   REPORTYP,SPOOKRTY                                                
         MVC   ARCCLASS,SPOOKARC                                                
         MVC   ARCTY1,SPOOKTY1                                                  
         MVC   ARCDOCTY,SPOOKADT                                                
         TM    SPOOKTY,REMOTDLQ+REMOTSQQ                                        
         BZ    *+8                                                              
         OI    OKREMOTE,X'04'      SET NO ARC                                   
*                                                                               
         USING REMOTED,RF                                                       
GJ18A    TM    OKREMOTE,X'01'      TEST PROFILE IN REMOTED                      
         BZ    GJ18D                                                            
         LA    RF,MYREMOTE                                                      
         TM    REMOTTYP,REMOTDLQ+REMOTSQQ                                       
         BZ    *+8                                                              
         OI    OKREMOTE,X'04'      SET NO ARC                                   
*                                                                               
GJ18B    CLI   REMOTRTY,C' '       TEST IF REPORT TYPE PROFILE                  
         BNH   GJ18B1              NO                                           
         CLI   REPORTYP,C'*'       YES TEST IF DEFAULT REQUESTED                
         BNE   *+8                                                              
         MVI   REPORTYP,C' '                                                    
GJ18B1   CLI   REPORTYP,C' '                                                    
         BH    *+10                                                             
         MVC   REPORTYP,REMOTRTY   USE PROFILE VALUE                            
*                                                                               
GJ18C    TM    OKREMOTE,X'04'      NO - ARCHIVE ?                               
         BO    GJ18X               YES, NO ARC=                                 
         CLI   REMOTARC,C' '       TEST IF ARCHIVE CLASS PROFILE                
         BNH   GJ18C1              NO                                           
         CLI   ARCCLASS,C'*'       YES TEST IF DEFAULT REQUESTED                
         BNE   *+8                                                              
         MVI   ARCCLASS,C' '                                                    
GJ18C1   CLI   ARCCLASS,C' '                                                    
         BH    *+10                                                             
         MVC   ARCCLASS,REMOTARC   USE PROFILE VALUE                            
         OC    ARCDOCTY,ARCDOCTY                                                
         BNZ   *+10                                                             
         MVC   ARCDOCTY,REMOTADT                                                
         CLI   ARCTY1,0                                                         
         BNE   *+10                                                             
         MVC   ARCTY1,REMOTTY1                                                  
*                                                                               
GJ18D    CLI   ARCCLASS,C'*'       TEST/SET DEFAULT ARCHIVE CLASS               
         BNE   *+8                                                              
         MVI   ARCCLASS,C'A'                                                    
         CLI   REPORTYP,C'*'       TEST/SET DEFAULT REPORT TYPE                 
         BNE   *+8                                                              
         MVI   REPORTYP,C' '                                                    
         CLC   SPOOKXT(3),=C'XT='     TEST IF EXTENDED SPOOK                    
         BNE   GJ18E                                                            
         MVC   SPOOKRTY,REPORTYP                                                
         MVC   SPOOKARC,ARCCLASS                                                
         MVC   SPOOKADT,ARCDOCTY                                                
         MVC   SPOOKTY1,ARCTY1                                                  
*                                                                               
GJ18E    CLI   ARCCLASS,C'A'       TEST IF ARCHIVABLE REPORT                    
         BL    GJ18X                                                            
         MVC   CARD,SPACES         DONT WANT PRINT SEGMENTATION                 
         MVC   CARD(15),=C'SEGMENT=9999999'                                     
         BRAS  RE,PUT                                                           
         L     RE,AARCCARD         ARC=AA,SPP,1S    ,DDDDD,UUUUU, ,C            
         A     RE,RELO                                                          
         MVC   CARD(80),0(RE)                                                   
         MVC   CARD+04(2),SPOOKAGY AGENCY                                       
*NOP*    MVC   CARD+07(3),SPOOKDID REQUESTOR'S INITIALS                         
         MVC   CARD+07(1),SPOOKSYS SYSTEM                                       
         MVC   CARD+08(2),SPOOKEOD PROGRAM                                      
         OC    REMOTFNO,REMOTFNO   FORMS CODE                                   
         BZ    *+10                                                             
         MVC   CARD+11(4),REMOTFNO                                              
         SR    R1,R1               DESTINATION ID NUMBER                        
         ICM   R1,3,SPOOKDES                                                    
         BNZ   *+8                                                              
         ICM   R1,3,SPOOKUID                                                    
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CARD+18(5),DUB                                                   
         SR    R1,R1               ORIGIN ID NUMBER                             
         ICM   R1,3,SPOOKUID                                                    
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CARD+24(5),DUB                                                   
         CLI   REPORTYP,C' '                                                    
         BNH   *+10                                                             
         MVC   CARD+30(1),REPORTYP REPORT TYPE                                  
         MVC   CARD+32(1),ARCCLASS ARCHIVE CLASS                                
         ICM   RE,15,ARCDOCTY      ARCHIVE DOCUMENT TYPE                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CARD+34(8),DUB                                                   
         MVC   CARD+33(9),SPACES   *NOP* DOCTYP                                 
         BRAS  RE,PUT                                                           
         OI    OKREMOTE,X'02'      SET ARC=CARD OUTPUT                          
GJ18X    EQU   *                                                                
         DROP  RF                                                               
*                                                                               
GJ19     L     RE,ADIRCARD         DIRECT=SPP,1S  ,IIIII,.....                  
         A     RE,RELO                                                          
         MVC   CARD(80),0(RE)                                                   
         MVC   CARD+07(3),SPOOKDID                                              
         TM    OKREMOTE,X'01'      PICK UP A(DATA) IN REMOTED FORMAT            
         BZ    GJ19B                                                            
         LA    RF,MYREMOTE         SET PROFILE VALUES INTO DIRECT=CARD          
         USING REMOTED,RF                                                       
         OC    REMOTDSC,REMOTDSC   REPORT SHORT DESCRIPTION                     
         BZ    *+10                                                             
         MVC   CARD+56(11),REMOTDSC                                             
         OC    REMOTSUB,REMOTSUB   REPORT 2 CHARACTER SUB CODE                  
         BZ    *+10                                                             
         MVC   CARD+68(2),REMOTSUB                                              
         OC    REMOTFNO,REMOTFNO   REPORT FORMS CODE                            
         BZ    *+10                                                             
         MVC   CARD+11(4),REMOTFNO                                              
         OC    REMOTCPY,REMOTCPY   NUMBER OF COPIES                             
         BZ    *+10                                                             
         MVC   CARD+32(1),REMOTCPY                                              
*                                                                               
GJ19B    MVI   CARD+28,C' '        ALWAYS PUT BLANK INTO PQCLASS                
         MVI   CARD+32,C'1'        TEMP FIX (ALWAYS GENERATE ONE COPY)          
         DROP  RF                                                               
*                                                                               
GJ19C    CLI   REPORTYP,C' '       REPORT TYPE                                  
         BNH   *+10                                                             
         MVC   CARD+26(1),REPORTYP                                              
         TM    OKREMOTE,X'04'      SET NO ARC                                   
         BO    GJ19D               YES                                          
         CLI   ARCCLASS,C' '       ARCHIVE CLASS                                
         BNH   *+10                                                             
         MVC   CARD+27(1),ARCCLASS                                              
         TM    ARCTY1,X'80'        ARCHIVE STATUS                               
         BZ    *+8                                                              
         MVI   CARD+29,C'E'        ARCE - ELIGIBLE                              
         TM    ARCTY1,X'40'                                                     
         BZ    *+8                                                              
         MVI   CARD+29,C'A'        ARCA - ARCHIVABLE                            
         TM    ARCTY1,X'20'                                                     
         BZ    *+8                                                              
         MVI   CARD+29,C'D'        ARCD - ARCHIVED                              
*                                                                               
GJ19D    LA    RF,SPOOKUID         SET USERID NUMBER                            
         NC    SPOOKDES,SPOOKDES                                                
         BZ    *+8                                                              
         LA    RF,SPOOKDES                                                      
         SR    R1,R1                                                            
         ICM   R1,3,0(RF)                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CARD+16(5),DUB                                                   
*                                                                               
GJ19E    CLC   SPOOKFRM(3),=C'FO=' SET FORM CODE IF DEFINED                     
         BNE   GJ19F                                                            
         MVC   CARD+11(4),SPOOKFRM+3                                            
         B     GJ19X                                                            
*                                                                               
GJ19F    CLC   SPOOKXT(3),=C'XT='  TEST IF EXTENDED SPOOK                       
         BNE   GJ19X                                                            
         MVC   CARD+22(2),=C'Q,'                                                
         CLC   SPOOKFNO,SPACES                                                  
         BNH   *+10                                                             
         MVC   CARD+11(4),SPOOKFNO FORM CODE                                    
         CLC   SPOOKSUB,SPACES                                                  
         BNH   *+10                                                             
         MVC   CARD+68(2),SPOOKSUB PQ FORM                                      
*                                                                               
GJ19G    TM    SPOOKTY,REMOTDLQ    TEST DOWN LOADABLE REPORT                    
         BNO   GJ19H                                                            
         MVI   CARD+24,C'D'                                                     
         B     GJ19I                                                            
*                                                                               
GJ19H    TM    SPOOKTY,REMOTSQQ    TEST IF TO BE CONVERTED TO SQL               
         BNO   GJ19I                                                            
         MVI   CARD+24,C'S'                                                     
         MVC   WORK(11),SPACES     C'SPP  @ABCDEF' IS SQL REPORT NAME           
         MVC   WORK(3),PROFKEY                                                  
         CLC   SPOOKSQL,SPACES     TEST IF CALLER PASSED TRANSFORM CODE         
         BNH   GJ19H1                                                           
         MVI   WORK+5,C'@'                                                      
         MVC   WORK+6(5),SPOOKSQL                                               
         B     GJ19H2                                                           
                                                                                
GJ19H1   CLC   MYREMOTE+REMOTSQL-REMOTED(5),SPACES                              
         BNH   GJ19H2                                                           
         MVI   WORK+5,C'@'                                                      
         MVC   WORK+6(5),MYREMOTE+REMOTSQL-REMOTED                              
GJ19H2   MVC   CARD+56(11),WORK    SET DESCRIPTION TO CONTAIN SQL INFO          
*                                                                               
GJ19I    TM    SPOOKSTA,X'02'      TEST INVISIBLE REPORT                        
         BNO   *+8                                                              
         MVI   CARD+25,C'I'        INVISIBLE                                    
*                                                                               
GJ19J    CLI   MYREMOTE+REMOTRET-REMOTED,0 TEST VALID RETAIN CLASS              
         BE    GJ19X                                                            
         MVC   CARD+70(1),MYREMOTE+REMOTRET-REMOTED                             
*                                                                               
GJ19X    BRAS  RE,PUT                                                           
*                                                                               
         CLI   SPOOKQTY,SPOOKQCS   COMSCORE?                                    
         BNE   GJ19Z                                                            
         L     RE,ACOMSCRE         COMSCORE= CARD                               
         A     RE,RELO                                                          
         MVC   CARD(80),0(RE)                                                   
         BRAS  RE,PUT                                                           
*                                                                               
         L     RE,AMVSDSN          MVSDSN= CARD                                 
         A     RE,RELO                                                          
         MVC   CARD(80),0(RE)                                                   
         BRAS  RE,PUT                                                           
*                                                                               
GJ19Z    CLI   MYREMOTE+REMOTRET-REMOTED,0                                      
         BE    GJ20                                                             
         MVC   CARD(7),=C'RETAIN=' SET RETAIN=X IF REMOTRET SET                 
         MVC   CARD+7(1),MYREMOTE+REMOTRET-REMOTED                              
         BRAS  RE,PUT                                                           
*                                                                               
GJ20     L     RF,AREQCRDS         A(REQUEST CARDS)                             
         CLI   0(RF),C'='          IF CARD BEGINS WITH EQUALS SIGN,             
         BNE   GJ25                THEN IT'S A CONTROL CARD                     
         MVC   CARD(79),1(RF)                                                   
         BRAS  RE,PUT              PUT THE CONTROL CARD WITHOUT '='             
         ZIC   R0,REQCON                                                        
         BCTR  R0,0                                                             
         STC   R0,REQCON           DECREMENT NUMBER OF REQUEST CARDS            
         LA    RF,80(RF)                                                        
         ST    RF,AREQCRDS         BUMP TO NEXT CARD                            
         B     GJ20                LOOK FOR MORE CONTROL CARDS                  
*                                                                               
GJ25     MVC   CARD(2),=C'/*'                                                   
         BRAS  RE,PUT                                                           
         B     GJY                                                              
*                                                                               
GJ30     CLC   SPOOKXT(3),=C'XT='  TEST IF EXTENDED SPOOK                       
         BNE   GJ33                                                             
         TM    SPOOKIND,SPOOKINB   TEST NOW USES NEW BILLING                    
         BNO   GJ33                                                             
         MVC   CARD(9),=C'LOAD=ACNB' YES, SET TO LOAD NEW BILLING               
GJ33     BRAS  RE,PUT                                                           
         B     GJ2                                                              
*                                                                               
GJN      LTR   RB,RB               SET CC=NEQ ON ERROR                          
         B     XIT                                                              
GJY      CR    RB,RB               SET CC=EQ IF OK                              
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*CHECK IF CONDITIONS                                                            
**********************************************************************          
IFTEST   CLC   CARD+3(7),=C'AGENCY='                                            
         BE    IFT2                                                             
         CLC   CARD+3(12),=C'DESTINATION='                                      
         BE    IFT4                                                             
         CLC   CARD+3(8),=C'HEXCOMP='                                           
         BE    IFT6                                                             
         CLC   CARD+3(7),=C'OUTPUT='                                            
         BE    IFT8                                                             
         CLC   CARD+3(6),=C'SUBID='                                             
         BE    IFT10                                                            
         CLC   CARD+3(6),=C'FACID='                                             
         BE    IFT12                                                            
         CLC   CARD+3(7),=C'SYSTEM='                                            
         BE    IFT14                                                            
         CLC   CARD+3(7),=C'ORIGIN='                                            
         BE    IFT16                                                            
         CLC   CARD+3(7),=C'DSPACE='                                            
         BE    IFT18                                                            
         B     GJ2                                                              
*                                                                               
IFT2     CLC   CARD+10(2),SPOOKAGY                                              
         BNE   *+8                                                              
         MVI   IF,C'Y'                                                          
         B     GJ2                                                              
*                                                                               
IFT4     MVC   DUB(2),SPOOKDES     DESTINATION=NNNNNN                           
         OC    DUB(2),DUB                                                       
         BNZ   *+10                                                             
         MVC   DUB(2),SPOOKUID                                                  
         LH    R0,DUB                                                           
         CVD   R0,DUB                                                           
         UNPK  WORK(6),DUB                                                      
         OI    WORK+5,X'F0'                                                     
         CLC   WORK(6),CARD+15                                                  
         BNE   *+8                                                              
         MVI   IF,C'Y'                                                          
         B     GJ2                                                              
*                                                                               
IFT6     GOTO1 HEXOUT,DMCB,SPOOKAGY,DUB,1                                       
         CLC   CARD+11(2),DUB                                                   
         BNE   *+8                                                              
         MVI   IF,C'Y'                                                          
         B     GJ2                                                              
*                                                                               
IFT8     L     RF,AREQCRDS         POINT TO REQUEST CARDS                       
         LA    R0,REQEOH-REQOUT    GET DISPLACEMENT IN HEADER                   
         LCR   R0,R0                                                            
         AR    RF,R0               RF NOW POINTS TO OUTPUT                      
         CLC   CARD+10(6),0(RF)                                                 
         BNE   *+8                                                              
         MVI   IF,C'Y'                                                          
         B     GJ2                                                              
*                                                                               
IFT10    CLC   CARD+9(3),SPOOKDID                                               
         BNE   *+8                                                              
         MVI   IF,C'Y'                                                          
         B     GJ2                                                              
*                                                                               
IFT12    CLC   CARD+9(1),FACID                                                  
         BNE   *+8                                                              
         MVI   IF,C'Y'                                                          
         B     GJ2                                                              
*                                                                               
IFT14    CLC   CARD+10(1),SPOOKSYS                                              
         BNE   GJ2                                                              
         CLI   CARD+11,C' '        TEST ONE CHARACTER SYSTEM                    
         BE    *+14                                                             
         CLC   CARD+11(1),SPOOKSYS+1 NO - MATCH SECOND CHARACTER TOO            
         BNE   GJ2                                                              
         MVI   IF,C'Y'                                                          
         B     GJ2                                                              
*                                                                               
IFT16    MVC   DUB(2),SPOOKUID     ORIGIN=NNNNNN                                
         LH    R0,DUB                                                           
         CVD   R0,DUB                                                           
         UNPK  WORK(6),DUB                                                      
         OI    WORK+5,X'F0'                                                     
         CLC   WORK(6),CARD+10                                                  
         BNE   *+8                                                              
         MVI   IF,C'Y'                                                          
         B     GJ2                                                              
*                                                                               
IFT18    CLC   CARD+10(1),DSPACE                                                
         BNE   *+8                                                              
         MVI   IF,C'Y'                                                          
         B     GJ2                                                              
*                                                                               
LASTJCL  NTR1  ,                                                                
         MVC   CARD,SPACES                                                      
         MVC   CARD(2),=C'**'                                                   
         BRAS  RE,PUT                                                           
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO OUTPUT THE CARDS                                                   
**********************************************************************          
PUT      NTR1  ,                                                                
         OC    ASPSV,ASPSV         TEST FIRST PUT                               
         BNZ   PUT3X               NO IRDR SAVE AREA ALREADY AVAIL              
         XC    JESKEY,JESKEY                                                    
         MVC   JESKUID,SPOOKDES    BUILD JES KEY                                
         OC    JESKUID,JESKUID                                                  
         BNZ   *+10                                                             
         MVC   JESKUID,SPOOKUID                                                 
         MVC   JESKDID,SPOOKDID                                                 
         MVI   JESKCLA,C'Q'                                                     
*                                                                               
         CLI   SPOOKWEN,5          TEST UPDATIVE SOON                           
         BNE   *+12                                                             
         OI    JESKTYPE,X'80'                                                   
         OI    JESKSTA,X'02'       MAKE ALL UPDATIVE SOON INVISIBLE             
         CLI   SPOOKWEN,7          TEST OVERNIGHT SOON                          
         BNE   *+8                                                              
         OI    JESKTYPE,X'04'      FLAG IT AS SUCH                              
         CLC   SPOOKRLD(3),=C'LD=' TEST IF LIVE/DEAD RETAIN SET                 
         BNE   PUT1                                                             
         MVC   JESKRETL,SPOOKRLD+3                                              
         MVC   JESKRETD,SPOOKRLD+5                                              
         B     PUT2                                                             
*                                                                               
PUT1     CLC   SPOOKXT(3),=C'XT='  TEST IF EXTENDED SPOOK PASSED                
         BNE   PUT2                                                             
         OC    SPOOKLH,SPOOKLH     LIVE HOURS PASSED                            
         BZ    *+10                                                             
         MVC   JESKRETL,SPOOKLH                                                 
         OC    SPOOKDH,SPOOKDH     DEAD HOURS PASSED                            
         BZ    *+10                                                             
         MVC   JESKRETD,SPOOKDH                                                 
PUT1A    TM    SPOOKTY,REMOTDLQ    TEST IF DOWNLOADABLE                         
         BZ    PUT1B                                                            
         OI    JESKTYPE,REMOTDLQ                                                
         B     PUT1C                                                            
*                                                                               
PUT1B    TM    SPOOKTY,REMOTSQQ    TEST IF TO BE CONVERTED TO SQL               
         BZ    *+8                                                              
         OI    JESKTYPE,REMOTSQQ                                                
         TM    SPOOKTY,X'02'       DO NOT UPDATE USERS JOB TABLE                
         BZ    *+8                                                              
         OI    JESKTYPE,X'02'                                                   
*                                                                               
PUT1C    CLC   SPOOKSUB,SPACES     PQ FORM PASSED IN SUB FIELD                  
         BNH   *+10                                                             
         MVC   JESKSUB,SPOOKSUB                                                 
         TM    SPOOKTY,REMOTDLQ+REMOTSQQ  DOWNLOAD OR SQL ?                     
         BNZ   PUT1F                      YES SO NO ARCV                        
*                                                                               
         CLI   REPORTYP,C'A'       PQ REPORT TYPE PASSED                        
         BL    *+10                                                             
         MVC   JESKRTY,REPORTYP                                                 
         CLI   ARCCLASS,C'A'       PQ ARCHIVE CLASS DEFINED                     
         BL    *+10                                                             
         MVC   JESKARC,ARCCLASS                                                 
         CLI   SPOOKTY1,0          PQ ARCHIVE FLAGS DEFINED                     
         BE    *+10                                                             
         MVC   JESKTYP1,SPOOKTY1                                                
*                                                                               
PUT1D    CLI   JESKARC,C'A'        SET ARCHIVABLE IF VALID ARC CLASS            
         BL    PUT1E                                                            
         CLI   JESKTYP1,0                                                       
         BNE   PUT1F                                                            
         OI    JESKTYP1,X'40'                                                   
         B     PUT1F                                                            
PUT1E    CLI   JESKTYP1,0          SET ARC CLASS IF ARCHIVABLE                  
         BE    PUT1F                                                            
         MVI   JESKARC,C'A'                                                     
*                                                                               
PUT1F    MVC   JESKPSWD,SPOOKPSW   SET REPORT SECURITY DATA                     
         MVC   JESKSEC1,SPOOKSF1                                                
         MVC   JESKSEC2,SPOOKSF2                                                
*                                                                               
PUT2     GOTOR GOPOWWOW,DMCB,=C'SCH',=C'ADR',JESKEY,ASPSV                       
         CLI   8(R1),0                                                          
         BE    PUT3                                                             
         XC    JESKEY,JESKEY                                                    
         LA    RF,JESKEY                                                        
         CLI   0(R9),X'FF'                                                      
         BNE   *+8                                                              
         ST    RF,20(R9)                                                        
         MVC   20(1,R9),8(R1)                                                   
         MVI   0(R9),2                                                          
         B     XIT                                                              
*                                                                               
PUT3     ICM   RE,15,ASPSV         PICK UP A(RETURNED SAVE AREA)                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   SPSV,0(RE)          COPY SAVE AREA TO MY WORKING STORAGE         
*                                                                               
PUT3X    BRAS  RE,PUTSCN           SCAN CARD AND EXTRACT CARD TYPE              
*                                                                               
PUT4     TM    JCLTYPE,EQUJOB      TEST JOB CARD                                
         BNO   PUT4X                                                            
         CLI   SPSVTCNT,0          TEST IF FIRST CARD                           
         BE    PUT4A                                                            
         MVC   SPSVJID,JCLNAME     SAVE JOB NAME OF SUBSEQUENT JOB              
         B     PUTW                                                             
PUT4A    MVC   SPSVJID,JCLNAME     SAVE JOB NAME OF FIRST JOB                   
         MVC   SPSVJCLA,JOBCLA     SAVE JOB CLASS                               
         MVC   SPSVJPRI,JOBPRI     SAVE JOB PRIORITY                            
         MVC   SPSVPCLA,PRTCLA     SAVE SYSLIST CLASS                           
         MVC   SPSVPFNO,PRTFNO     SAVE SYSLST FNO                              
         B     PUTW                                                             
PUT4X    EQU   *                                                                
*                                                                               
PUT5     TM    JCLTYPE,EQUEXEC     TEST EXEC CARD                               
         BNO   PUT5X                                                            
         ZIC   RE,SPSVXCNT         BUMP EXEC CARD COUNT                         
         LA    RE,1(RE)                                                         
         STC   RE,SPSVXCNT                                                      
*                                                                               
PUT5A    CLC   JCLNAME,=CL8'XXXXX'                                              
         BNE   PUT5B                                                            
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
         LA    R1,CARD                                                          
         MVC   2(4,R1),SPSVJID     SET //XXXXX  TO //JJJJNNN                    
         MVC   6(3,R1),DUB                                                      
*                                                                               
PUT5B    SR    RE,RE               TEST IF WANT PRTQ/QDQ DD CARDS               
         IC    RE,SYSID                                                         
         MHI   RE,3                                                             
         LA    RE,DQPQTAB(RE)                                                   
         MVC   DQDDN(3),0(RE)      EXTRACT QDQ DD AND PRTQ DD NUMS              
         OC    DQDDN(3),DQDDN      TEST IF FAC SYS NEEDS TST DD CARDS           
         BZ    PUTW                NO                                           
         CLI   PRTQ,C'Y'           BUT DO WE WANT THEM                          
         BNE   PUTW                NO                                           
         MVC   JESCARD,CARD        YES OUTPUT THE EXEC CARD HERE                
         GOTOR GOPOWWOW,DMCB,=C'SCH',=C'JES',JESKEY,JESCARD                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,SPSVTCNT         BUMP TOTAL CARD COUNT                        
         LA    RE,1(RE)                                                         
         STC   RE,SPSVTCNT                                                      
*                                                                               
PUT5B1   CLI   DQDDN,0             TEST IF WANT SPECIAL //DDSQDQ                
         BE    PUT5B2              NO                                           
         SR    RE,RE               YES INDEX INTO LIST OF QDQ DD CARDS          
         IC    RE,DQDDN                                                         
         BCTR  RE,0                                                             
         MH    RE,=H'80'                                                        
         LA    RE,QDQDD1(RE)                                                    
         MVC   CARD,0(RE)                                                       
         MVC   JESCARD,CARD                                                     
         GOTOR GOPOWWOW,DMCB,=C'SCH',=C'JES',JESKEY,JESCARD                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,SPSVTCNT         BUMP TOTAL CARD COUNT                        
         LA    RE,1(RE)                                                         
         STC   RE,SPSVTCNT                                                      
*                                                                               
PUT5B2   CLI   DCDDN,0             TEST IF WANT SPECIAL //DDSQDC                
         BE    PUT5B3              NO                                           
         SR    RE,RE               YES INDEX INTO LIST OF QDC DD CARDS          
         IC    RE,DCDDN                                                         
         BCTR  RE,0                                                             
         MHI   RE,80                                                            
         LA    RE,QDCDD1(RE)                                                    
         MVC   CARD,0(RE)                                                       
         MVC   JESCARD,CARD                                                     
         GOTOR GOPOWWOW,DMCB,=C'SCH',=C'JES',JESKEY,JESCARD                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,SPSVTCNT         BUMP TOTAL CARD COUNT                        
         LA    RE,1(RE)                                                         
         STC   RE,SPSVTCNT                                                      
*                                                                               
PUT5B3   CLI   PQDDN,0             TEST IF WANT SPECIAL //PRTQN                 
         BE    PUTX                NO                                           
         SR    RE,RE               YES INDEX INTO LIST OF PRTQ DD CARDS         
         IC    RE,PQDDN                                                         
         BCTR  RE,0                                                             
         MHI   RE,80                                                            
         LA    RE,PRTQDD1(RE)                                                   
         MVC   CARD,0(RE)                                                       
         LHI   R0,1                R0=PRTQ NUMBER                               
PUT5B4   LR    RF,R0                                                            
         BCTR  RF,0                                                             
         LA    RF,IDSPQS(RF)       RF=A(PRTQ LETTER)                            
         MVC   CARD+06(1),0(RF)                                                 
         MVC   CARD+28(1),0(RF)                                                 
         MVC   JESCARD,CARD                                                     
         GOTOR GOPOWWOW,DMCB,=C'SCH',=C'JES',JESKEY,JESCARD                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,SPSVTCNT         BUMP TOTAL CARD COUNT                        
         LA    RE,1(RE)                                                         
         STC   RE,SPSVTCNT                                                      
         AHI   R0,1                BUMP PRTQ FILE NUMBER                        
         CH    R0,NUMPQS                                                        
         BNH   PUT5B4                                                           
         B     PUTX                                                             
PUT5X    EQU   *                                                                
*                                                                               
PUT6     TM    JCLTYPE,EQUDD       TEST DD CARD                                 
         BNO   PUT6X                                                            
PUT6A    CLC   JCLNAME,=CL8'SYSPRINT'                                           
         BNE   PUT6B                                                            
         CLC   CARD(40),MVSLST                                                  
         BNE   PUT6B                                                            
*&&UK                                                                           
         CLC   SPOOKDID,=C'DDS'                                                 
         BE    *+12                                                             
         CLI   SPOOKSYS,C'A'       ACC REGISTERS                                
         BNE   PUT6B               GET LIST CLASS AND GOOD FNO                  
*&&                                                                             
         CLI   SPSVPCLA,C' '                                                    
         BE    PUT6B                                                            
         LA    RE,CARD+22          POINT TO END OF SYSOUT=                      
         MVC   0(4,RE),=C'(C,,'                                                 
         MVC   1(1,RE),SPSVPCLA                                                 
         MVC   4(4,RE),SPSVPFNO                                                 
         LA    RE,4(RE)                                                         
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C')'                                                       
         B     PUT6X                                                            
*                                                                               
PUT6B    CLC   JCLNAME,=CL8'PRTQUE'                                             
         BE    PUTX                DELETE OLD STYLE PRTQUE DD CARDS             
*                                                                               
PUT6X    EQU   *                                                                
*                                                                               
PUTW     MVC   JESCARD,CARD                                                     
         GOTOR GOPOWWOW,DMCB,=C'SCH',=C'JES',JESKEY,JESCARD                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,SPSVTCNT         BUMP TOTAL CARD COUNT                        
         LA    RE,1(RE)                                                         
         STC   RE,SPSVTCNT                                                      
*                                                                               
PUTX     MVC   CARD,SPACES                                                      
         B     XIT                                                              
         EJECT                                                                  
PUTSCN   NTR1                                                                   
         MVI   JCLTYPE,0           INITIALISE JCL CARD DATA                     
         MVI   JCLSTYPE,0                                                       
         XC    JCLOPRD,JCLOPRD                                                  
         MVC   JCLNAME,SPACES                                                   
         MVC   JCLACTN,SPACES                                                   
         LA    RE,CARD                                                          
*                                                                               
PUTSCN1  CLC   0(2,RE),=C'//'      TEST JCL STATEMENT                           
         BNE   PUTSCNX                                                          
         CLI   2(RE),C'*'          TEST COMMENT/JES3 STATEMENT                  
         BNE   PUTSCN5                                                          
         CLC   3(7,RE),=CL8'FORMAT'                                             
         BNE   *+12                                                             
         OI    JCLTYPE,EQUJFORM    SET JES3 //*FORMAT ...                       
         B     PUTSCNX                                                          
         CLC   3(5,RE),=CL8'MAIN'                                               
         BNE   *+12                                                             
         OI    JCLTYPE,EQUJMAIN    SET JES3 //*MAIN ...                         
         B     PUTSCNX                                                          
         CLC   3(4,RE),=CL8'NET'                                                
         BNE   *+12                                                             
         OI    JCLTYPE,EQUJNET     SET JES3 //*NET ...                          
         B     PUTSCNX                                                          
         OI    JCLTYPE,EQUCOMM     SET JCL COMMENT                              
         B     PUTSCNX                                                          
*                                                                               
PUTSCN5  CLC   2(40,RE),SPACES     CARD IS MVS JCL CARD                         
         BNE   *+12                                                             
         OI    JCLTYPE,EQUEOJ      SET EOJ STATEMENT                            
         B     PUTSCNX                                                          
         LA    R1,CARD+2                                                        
*                                                                               
PUTSCN6  LA    R0,9                SCAN FOR //XXXXXXXX ....                     
         LR    RF,R1                                                            
PUTSCN6C CLI   0(RF),C' '                                                       
         BE    PUTSCN6D                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,*-12                                                          
         B     PUTSCNX                                                          
PUTSCN6D LR    RE,RF               SAVE ADDR OF FIRST BLANK                     
         SR    RF,R1                                                            
         CH    RF,=H'8'                                                         
         BH    PUTSCNX                                                          
         SH    RF,=H'1'                                                         
         BM    PUTSCN6E                                                         
         EX    RF,*+8                                                           
         B     PUTSCN6E                                                         
         MVC   JCLNAME(0),0(R1)    SAVE IN JCLNAME                              
PUTSCN6E EQU   *                                                                
*                                                                               
PUTSCN7  LR    RF,RE               SCAN FOR //........ XXXX                     
         LA    R0,20                                                            
PUTSCN7A CLI   0(RF),C' '          POSITION TO FIRST CHR IN ACTION              
         BNE   PUTSCN7B                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,PUTSCN7A                                                      
         B     PUTSCNX                                                          
PUTSCN7B LA    R0,9                                                             
         LR    R1,RF               SAVE ADDR OF FIRST CHR IN ACTION             
PUTSCN7C CLI   0(RF),C' '                                                       
         BE    PUTSCN7D                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,*-12                                                          
         B     PUTSCNX                                                          
PUTSCN7D LR    RE,RF               SAVE ADDR OF FIRST CHR AFTER ACTION          
         SR    RF,R1                                                            
         CH    RF,=H'8'                                                         
         BH    PUTSCNX                                                          
         SH    RF,=H'1'                                                         
         BM    PUTSCN7E                                                         
         EX    RF,*+8                                                           
         B     PUTSCN7E                                                         
         MVC   JCLACTN(0),0(R1)    SAVE JCL ACTION                              
PUTSCN7E LR    RF,RE                                                            
         LA    R0,8                                                             
PUTSCN7F CLI   0(RF),C' '          POSITION TO FIRST CHR AFTER ACTION           
         BNE   PUTSCN7G                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,PUTSCN7F                                                      
         B     PUTSCN8                                                          
PUTSCN7G LA    RE,CARD             SAVE DISP TO FIRST OPERAND                   
         SR    RF,RE                                                            
         STCM  RE,3,JCLOPRD                                                     
*                                                                               
PUTSCN8  CLC   JCLACTN,=CL8'JOB'                                                
         BNE   *+12                                                             
         OI    JCLTYPE,EQUJOB      SET MVS //......... JOB ...                  
         B     PUTSCNX                                                          
         CLC   JCLACTN,=CL8'EXEC'                                               
         BNE   *+12                                                             
         OI    JCLTYPE,EQUEXEC     SET MVS //......... EXEC ...                 
         B     PUTSCNX                                                          
         CLC   JCLACTN,=CL8'DD'                                                 
         BNE   *+12                                                             
         OI    JCLTYPE,EQUDD       SET MVS //......... DD ....                  
         B     PUTSCNX                                                          
*                                                                               
PUTSCNX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* FIND A SPARE ENTRY IN THE JOB TABLE                                 *         
* EXIT - TCBSJENT CONTAINS EITHER A(ENTRY) OR DISPLACEMENT TO ENTRY   *         
*        IF TABLE IS FULL NULLS ARE RETURNED                          *         
***********************************************************************         
FINDNTRY NTR1  ,                                                                
         GOTO1 PROTOFF             TURN OFF FACPAK PROTECTION                   
*                                                                               
         L     RF,ASSB                                                          
         ICM   R3,15,SSBTKADR-SSBD(RF)                                          
         USING TCBD,R3                                                          
         XC    TCBSJENT,TCBSJENT                                                
*                                                                               
         BRAS  RE,SSET             STOP THE TIMER                               
         BRAS  RE,LOCKJOB          LOCK THE JOB TABLE                           
*                                                                               
         BRAS  RE,ARSOFF                                                        
         ICM   R2,15,AJTFRST       GET A(JOBTAB HEADER)                         
         LAM   AR2,AR2,TBLET                                                    
         SAC   512                                                              
         USING TABJOBS,R2                                                       
         MVC   JOBHDRL,TBJLHEAD    SET SOFT LENGTHS                             
         MVC   JOBTABL,TBJLNTRY                                                 
         AH    R2,JOBHDRL          BUMP PAST HEADER                             
         USING TBJNTRY,R2                                                       
         LH    RE,JOBTABL          SET UP FOR LOOP                              
         ICM   RF,15,AJTLAST                                                    
         OC    TBJCLASS(4),TBJCLASS                                             
         BZ    FNDN02                                                           
         BXLE  R2,RE,*-10          BUMP TO NEXT                                 
         B     FNDN04              MUST BE FULL                                 
*                                                                               
FNDN02   MVC   TBJCLASS(4),=XL4'FFFFFFFF'                                       
         ST    R2,TCBSJENT                                                      
*                                                                               
         L     R2,AJTFRST                                                       
         USING TBJOBHDR,R2                                                      
         ICM   RF,15,TBJNUM                                                     
         AHI   RF,1                BUMP NUMBER OF ENTRIES                       
         STCM  RF,15,TBJNUM                                                     
*                                                                               
FNDN04   BRAS  RE,ARSOFF                                                        
         BRAS  RE,FREEJOB                                                       
         BRAS  RE,RSET             START THE TIMER                              
*                                                                               
FINDNX   GOTO1 PROTON              TURN ON FACPAK PROTECTION                    
         BRAS  RE,ARSOFF                                                        
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* FILL IN THE REST OF THE JOB TABLE ENTRY WITH PERTINENT INFORMATION  *         
***********************************************************************         
FILLNTRY NTR1  ,                                                                
         MVI   WRITENO,NO          DOUBLE NEGATIVE                              
         GOTO1 PROTOFF             TURN OFF FACPAK PROTECTION                   
         BRAS  RE,SSET             STOP THE TIMER                               
         BRAS  RE,LOCKCLS                                                       
         BRAS  RE,LOCKJOB                                                       
         BRAS  RE,ARSOFF                                                        
*                                                                               
         L     R4,ASSB                                                          
         USING SSBD,R4                                                          
         L     RF,SSBTKADR         A(TCB ENTRY)                                 
         USING TCBD,RF                                                          
         L     R2,TCBSJENT         A(THIS JOB TABLE ENTRY)                      
         LAM   AR2,AR2,TBLET                                                    
         SAC   512                                                              
         USING TBJOBTAB,R2                                                      
         MVC   TBJADV,SSBSYSIX     ADV                                          
         MVC   TBJPQKEY,TCBSJUSR   PQ KEY                                       
*&&US                                                                           
         LA    R1,SOONLST          SET FROM SPECIAL LIST                        
FILN00   CLI   10(R1),C'?'                                                      
         BE    FILN01                                                           
         CLC   SPOOKSYS(1),10(R1)                                               
         BE    FILN01                                                           
         AHI   R1,L'SOONLST                                                     
         LA    R0,SOONLSTX                                                      
         CR    R1,R0                                                            
         BNH   FILN00                                                           
         DC    H'0'                                                             
*                                                                               
FILN01   MVC   TBJCLASS,8(R1)      SET CLASS                                    
*&&                                                                             
*&&UK                                                                           
         MVC   TBJCLASS(1),JOBCLA  SAVE SUBMIT CLASS                            
         MVI   TBJCLASS+1,C'A'     DEFAULT TO ADV CLASS+1                       
*                                                                               
         TM    SSBSYSFL,SSBSYTST   TEST SYSTEM?                                 
         BZ    *+10                                                             
         MVC   TBJCLASS+1,SSBSYSN1 SET SYSTEM CHR AS CLASS+1                    
*                                                                               
         CLI   TBJCLASS,C' '       FINAL TEST FOR ZERO CLASS                    
         BH    *+8                                                              
         MVI   TBJCLASS,C'X'       ZERO CLASS GETS CLASS X                      
*                                                                               
         CLI   TBJCLASS,C'X'       CLASS X OR ZERO SET TO A CLASS               
         BNE   *+8                                                              
         MVI   TBJCLASS,C'A'                                                    
*&&                                                                             
         MVI   TBJSTAT,TBJINUSE    FLAG AS IN USE                               
         CLI   SPOOKWEN,5          TEST UPDATIVE SOON                           
         BNE   *+8                                                              
         OI    TBJSTAT,TBJUPDT     FLAG AS SUCH                                 
         MVC   TBJPRTY,JOBPRI      PRIORITY                                     
         CLI   TBJPRTY,C'0'                                                     
         BH    *+8                                                              
         MVI   TBJPRTY,C'5'                                                     
         XC    TBJNEXT,TBJNEXT                                                  
         XC    TBJPREV,TBJPREV                                                  
         XC    TBJNXT,TBJNXT                                                    
         XC    TBJRTIME,TBJRTIME                                                
         XC    TBJETIME,TBJETIME                                                
         XC    TBJMONS,TBJMONS                                                  
*                                                                               
         MVI   QTYPE,SPOOKQMD      DEFAULT IS MEDIUM                            
*&&US                                                                           
         CLC   SPOOKXT(3),=C'XT='  TEST IF EXTENDED SPOOK PASSED                
         BNE   FILN01A                                                          
         CLI   SPOOKQTY,SPOOKQCS   COMSCORE QUEUE TYPE?                         
         BNE   *+8                 NO                                           
         MVI   QTYPE,SPOOKQCS                                                   
*                                                                               
         CLI   SPOOKQTY,SPOOKQLG   LONG RUNNING SOON QUEUE TYPE?                
         BNE   FILN01A             NO                                           
         MVI   QTYPE,SPOOKQLG                                                   
         OI    TBJSTAT,TBJLONG                                                  
*&&                                                                             
FILN01A  MVC   TBJCTYPE,QTYPE      JOB QUEUE CLASS TYPE                         
*                                                                               
         USING UTLD,R3                                                          
FILN01B  L     R3,TCBUTL           A(UTL ENTRY)                                 
         OC    OVRDUTL(2),OVRDUTL  TEST OVERRIDE UTL ENTRY                      
         BZ    *+8                                                              
         LA    R3,OVRDUTL                                                       
         MVC   TBJLUID,TSYM        TERMINAL LUID                                
         MVC   TBJTERM,TNUM        TERMINAL NUM                                 
         MVC   TBJAGY,TAGY         AGENCY                                       
         DROP  RF                                                               
*                                                                               
         XR    RE,RE                                                            
         CLC   TUSER,=AL2(32000)   PUBLIC REPORTS GO TO FIRST PRTQUE            
         BL    *+14                                                             
         CLC   TUSER,=AL2(32100)                                                
         BNH   FILN02                                                           
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,TBJPQUSR       BUG FIX - WAS TUSER                          
         LH    R0,NUMPQS                                                        
         DR    RE,R0                                                            
*                                                                               
FILN02   LA    RE,IDSPQS(RE)                                                    
         MVC   TBJPQID,0(RE)       EBCDIC PRTQ ID                               
         BRAS  RE,ARSOFF                                                        
         L     RF,AUTL                                                          
         TM    TSTAT1-UTLD(RF),TSTATDDS                                         
         BZ    FILN02A                                                          
*                                                                               
         L     RF,ASSB                                                          
         TM    SSBSYSFL-SSBD(RF),SSBSYCSC                                       
         BO    FILN04              LET IT GO                                    
*                                                                               
FILN02A  CLC   =C'HH',TAGY         H&K         HH                               
         BE    FILN02C                                                          
         CLC   =C'HK',TAGY         H&K         HK                               
         BNE   FILN02R             NEXT TEST                                    
*                                                                               
FILN02C  CLI   SPOOKWEN,5          TEST UPDATIVE SOON                           
         BE    FILN03              NO UPDATIVE REPORTS                          
         B     FILN04              NOP TO DEACTIVATE                            
*                                                                               
FILN02R  CLC   TUSER,=AL2(13998)   MVREAD   H9                                  
         BE    FILN03              NO SOONS                                     
         CLC   TUSER,=AL2(13997)   STAREAD  H9                                  
         BE    FILN03              NO SOONS                                     
         CLC   TUSER,=AL2(13999)   SPAREAD  H9                                  
         BE    FILN03              NO SOONS                                     
         CLC   TUSER,=AL2(14000)   MVNREAD  DU                                  
         BE    FILN03              NO SOONS                                     
         CLC   TUSER,=AL2(14001)   ARMYREAD J2                                  
         BE    FILN03              NO SOONS                                     
*                                                                               
         CLC   =C'TSO',JESKDID     IGNORE 'TSO' FROM DDS TERMINAL               
         BNE   FILN04                                                           
         L     RF,AUTL                                                          
         TM    TSTAT1-UTLD(RF),TSTATDDS                                         
         BZ    FILN04                                                           
*                                                                               
FILN03   MVI   RUNJOB,NO                                                        
         B     FILLXIT             'RESET' MAKES THESE SHOW UP AND RUN          
*                                                                               
FILN04   BRAS  RE,FILLCLS          FILL CLASS TABLE USING SHADOW                
*                                                                               
FILLXIT  BRAS  RE,ARSOFF                                                        
         BRAS  RE,FREEJOB                                                       
         BRAS  RE,FREECLS                                                       
         BRAS  RE,ARSOFF                                                        
         OI    SSBSTAT1,SSBSCHK1                                                
         BRAS  RE,RSET                                                          
         GOTO1 PROTON                                                           
         B     XIT                                                              
*                                                                               
       ++INCLUDE FASOONLST                                                      
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FILL IN CLASS INFORMATION IN NEW CLASS TABLE IN TABS     *         
* NTRY: TCBSJENT = A(JOBTAB ENTRY FOR THIS TASK)                      *         
***********************************************************************         
FILLCLS  NTR1  ,                                                                
         BRAS  RE,ARSOFF                                                        
         ICM   R2,15,ACLFRST                                                    
         LAM   AR2,AR2,TBLET                                                    
         SAC   512                                                              
         USING JCLASSD,R2                                                       
*                                                                               
FCL02    L     R4,ASSB             OLD ENTRY FILLER                             
         USING SSBD,R4                                                          
         L     RF,SSBTKADR         A(TCB ENTRY)                                 
         USING TCBD,RF                                                          
*                                                                               
         L     R3,TCBSJENT         DISP TO JOB TABLE ENTRY                      
         LAM   AR3,AR3,TBLET                                                    
         USING TBJOBTAB,R3                                                      
         DROP  RF                                                               
*                                                                               
FCL04    OC    JCLASSD(JCKEYL),JCLASSD                                          
         BZ    FCL08                                                            
*&&CL*&& CLC   JCCLASS,TBJCLASS    CLASS                                        
*&&CL*&& BNE   FCL06                                                            
*&&AG*&& CLC   JCAGY,TBJAGY        AGENCY                                       
*&&AG*&& BNE   FCL06                                                            
         CLC   QTYPE,JCTYPE        QUEUE TYPE                                   
         BNE   FCL06                                                            
         B     FCL08                                                            
*                                                                               
FCL06    AHI   R2,JCLASSL          NEXT CLASS                                   
         C     R2,ACLLAST                                                       
         BL    FCL04                                                            
         DC    H'0'                CLASS TABLE IS FULL - NOT GOOD               
*                                                                               
FCL08    DS    0H                                                               
*&&CL*&& MVC   JCCLASS,TBJCLASS                                                 
*&&AG*&& MVC   JCAGY,TBJAGY                                                     
         MVC   JCTYPE,QTYPE                                                     
*                                                                               
         XR    R0,R0               INCREMENT # SUB                              
         ICM   R0,3,JCNSUB                                                      
         AHI   R0,1                                                             
         STCM  R0,3,JCNSUB                                                      
         XR    R0,R0               INCREMENT TOTAL # SUB                        
         ICM   R0,3,JCNTSUB                                                     
         AHI   R0,1                                                             
         STCM  R0,3,JCNTSUB                                                     
*                                                                               
         OC    JCFSUB,JCFSUB       ANYTHING IN QUEUE?                           
         BNZ   FCL10               YES                                          
         STCM  R3,15,JCFSUB                                                     
         STCM  R3,15,JCLSUB                                                     
         B     FILLCLSX                                                         
*                                                                               
FCL10    LR    R0,R3               SAVE A(THIS ENTRY)                           
         ICM   R3,15,JCLSUB        LINKED LIST IS OK?                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STCM  R0,15,JCLSUB        SET A(END OF LIST)                           
         STCM  R0,15,TBJNXT        SET A(NEXT IN LIST)                          
*                                                                               
FILLCLSX BRAS  RE,ARSOFF                                                        
         B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* LOCK AND UNLOCK JOB TABLE ENTRY                                     *         
***********************************************************************         
LOCKJOB  NTR1  ,                                                                
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTJOB)                                               
         GOTO1 LOCKSPC,DUB                                                      
         B     XIT                                                              
*                                                                               
FREEJOB  NTR1  ,                                                                
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTJOB)                                               
         MVI   DUB,X'10'                                                        
         GOTO1 LOCKSPC,DUB                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LOCK AND UNLOCK CLASS TABLE ENTRY                                   *         
***********************************************************************         
LOCKCLS  NTR1  ,                                                                
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTDCLASS)                                            
         GOTO1 LOCKSPC,DUB                                                      
         B     XIT                                                              
*                                                                               
FREECLS  NTR1  ,                                                                
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTDCLASS)                                            
         MVI   DUB,X'10'                                                        
         GOTO1 LOCKSPC,DUB                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
INIT     NTR1  ,                                                                
         MVI   RUNJOB,YES                                                       
         XC    ASPSV,ASPSV                                                      
         XC    SPSV,SPSV           CLEAR SPOOL SAVE AREA                        
         XC    OVRDUTL,OVRDUTL     CLEAR OVERRIDING UTL DATA                    
*                                                                               
         MVC   REQCON,4(R9)        SAVE REQUEST CONTROL BYTE                    
         L     R1,4(R9)            SAVE REQUEST CARD                            
         MVC   REQ,0(R1)                                                        
         ST    R1,AREQCRDS         SAVE A(REQUEST CARD)                         
*                                                                               
         L     R1,12(R9)                                                        
         MVC   BOOK,0(R1)          SAVE JCL BOOK NAME                           
         CLC   BOOK(2),=C'NE'      TEST BUILT NETWORK BOOK NAME                 
         BNE   *+10                                                             
         MVC   BOOK(2),=C'SP'      FORCE TO SPOT BOOK                           
*                                                                               
*&&UK*&& CLI   SPOOKSYS,C'M'       ENSURE MEDLINE IS C'ME'                      
*&&UK*&& BNE   INIT02                                                           
*&&UK*&& CLI   SPOOKSYS+1,C'B'     MB FOR MEDIABASE                             
*&&UK*&& BE    INIT02                                                           
*&&UK*&& CLI   SPOOKSYS+1,C'P'     MP FOR MEDIA PLANNING                        
*&&UK*&& BE    INIT02                                                           
*&&UK*&& MVI   SPOOKSYS+1,C'E'                                                  
*                                                                               
         USING COMFACSD,RF                                                      
INIT02   L     RF,8(R9)            RF=A(COMFACS)                                
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   HEXOUT,CHEXOUT                                                   
         MVC   SWITCH,CSWITCH                                                   
         MVC   PQPROF,CPQPROF                                                   
         MVC   PROTON,CPROTON                                                   
         MVC   PROTOFF,CPROTOFF                                                 
         MVC   LOCKSPC,CLOCKSPC                                                 
         MVC   GETFACT,CGETFACT                                                 
         MVC   DATCON,CDATCON                                                   
         ICM   RE,15,=V(PQPROF)                                                 
         BZ    *+12                                                             
         A     RE,RELO                                                          
         ST    RE,PQPROF                                                        
         DROP  RF                                                               
*                                                                               
         GOTO1 GETFACT,DMCB,0      A(FACPAK EXTRA DATA)                         
*                                                                               
         USING FACTSD,RF                                                        
         L     RF,0(R1)                                                         
         MVC   TODAY,FADATE                                                     
         MVC   TODAYB,FADATEB                                                   
         MVC   POWWOW,FAPOWWOW                                                  
         MVC   SYSID,FASYSID       SAVE FACPAK SYSTEM ID NUMBER                 
         MVC   DSPACE,FADSPACE     SAVE DSPACE                                  
         MVC   FILSYS,FAOVSYS      SAVE SYSTEM NUMBER                           
         MVC   FILSEN,FASYS        SAVE SYSTEM SE NUMBER                        
         DROP  RF                                                               
*                                                                               
         XC    WORK(5),WORK        FIND CORRISPONDING SYSTEM ID                 
         MVC   WORK(3),=C'SE='                                                  
         MVC   WORK+4(1),FILSEN    MOVE IN SE#                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DDNAME'),WORK,0                               
         TM    8(R1),X'10'                                                      
         BO    INIT03              SYSTEM NOT FOUND, JUST SKIP                  
*                                                                               
         L     RF,8(,R1)           A(DDNADATA)                                  
         USING DDNAMED,RF                                                       
         MVC   SYSNID,DDNASEID     THE 'A' IN SPOTA,'IT' IN SPOTIT              
         DROP  RF                                                               
*                                                                               
INIT03   GOTO1 DATCON,DMCB,(15,0),(10,BILLDATE)                                 
         GOTO1 DATCON,DMCB,(3,TODAYB),(1,TODAYP)                                
*                                                                               
         L     RF,=V(GETBOOK)                                                   
         A     RF,RELO                                                          
         ST    RF,GETBOOK                                                       
         L     RF,=V(REPLACE)                                                   
         A     RF,RELO                                                          
         ST    RF,REPLACE                                                       
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         GOTO1 SWITCH,DMCB,X'FEFFFFFF'                                          
         L     RF,DMCB                                                          
         ST    RF,ASYSFAC          SAVE A(SYSFACS)                              
         USING SYSFACD,RF                                                       
         MVC   ATICTOC,VTICTOC                                                  
         MVC   ASSB,VSSB                                                        
*                                                                               
         L     RF,ASSB                                                          
         USING SSBD,RF                                                          
         MVC   FACID,SSBSYSN1                                                   
         MVC   FACNAM,SSBSYSNA                                                  
         MVC   FACFLG,SSBSYSFL                                                  
         MVC   TBLET,SSBTBLET                                                   
         DROP  RF                                                               
*                                                                               
         GOTO1 SWITCH,DMCB,X'FFFFFFFF'                                          
         L     RF,DMCB                                                          
         ST    RF,AUTL             SAVE A(UTL ENTRY)                            
*                                                                               
         BRAS  RE,SETDSP                                                        
*                                                                               
         TM    16(R9),X'80'        TEST USE SPOOK TRM                           
         BZ    INIT04              NO                                           
*                                                                               
         SAM31                     YES COPY UTL FROM 31-BIT STORAGE             
         L     RF,ASYSFAC                                                       
         L     RF,VUTL-SYSFACD(RF)                                              
         SR    RE,RE                                                            
         ICM   RE,3,SPOOKTID                                                    
         BCTR  RE,0                                                             
         MH    RE,0(RF)            TRMNUM X ENTRY LEN                           
         LA    RE,6(RE,RF)         POINT TO UTL ENTRY                           
         MVC   OVRDUTL,0(RE)       SAVE IT IN MY STORAGE                        
         SAM24                                                                  
*                                                                               
INIT04   NC    SPOOKUID,SPOOKUID                                                
         BNZ   *+10                                                             
         MVC   SPOOKUID,ID                                                      
         NC    SPOOKEOD,SPOOKEOD                                                
         BNZ   *+10                                                             
         MVC   SPOOKEOD,REQ                                                     
         NC    SPOOKAGY,SPOOKAGY                                                
         BNZ   *+10                                                             
         MVC   SPOOKAGY,AGENCY                                                  
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET TBJCLASS CORRECTLY REGARDLESS OF MAINCLAS CONTENTS   *         
* COPIED FROM BEFR ROUTINE - CHANGE THAT, CHANGE THIS ALSO            *         
***********************************************************************         
SETJTCLS NTR1  ,                                                                
         CLI   SPOOKSYS,C'A'        SCOPE FOR RANGE                             
         BNL   *+6                  THIS IS FOR DEBUGGING                       
         DC    H'0'                                                             
         CLI   SPOOKSYS,C'9'                                                    
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   MAINCLAS,SPOOKSYS                                                
         MVC   MAINCLAS+1(2),SYSNID                                             
*&&US                                                                           
         CLI   SPOOKSYS,C'C'       CONTROL                                      
         BNE   SJT06                                                            
         MVC   MAINCLAS+1(2),=C'1 '                                             
*&&                                                                             
SJT06    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET ADDRESSES FOR THE 3 TABLES IN THE TABS DATASPACE     *         
***********************************************************************         
SETDSP   NTR1  ,                                                                
         GOTO1 PROTOFF                                                          
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTDCLASS)                                            
         MVI   DUB,X'20'           ENQUIRE                                      
         GOTO1 LOCKSPC,DUB                                                      
         ICM   RF,15,4(R1)                                                      
         USING DMSPACED,RF                                                      
         MVC   ACLFRST,DSPTFRST-DMSPACED(RF)                                    
         NI    ACLFRST,X'3F'                                                    
         MVC   ACLLAST,DSPTEND-DMSPACED(RF)                                     
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTJOB)                                               
         MVI   DUB,X'20'           ENQUIRE                                      
         GOTO1 LOCKSPC,DUB                                                      
         ICM   RF,15,4(R1)                                                      
         USING DMSPACED,RF                                                      
         MVC   AJTFRST,DSPTFRST-DMSPACED(RF)                                    
         NI    AJTFRST,X'3F'                                                    
         MVC   AJTLAST,DSPTEND-DMSPACED(RF)                                     
         BRAS  RE,ARSOFF                                                        
         GOTO1 PROTON                                                           
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* COMMON INTERFACE TO POWWOW TO SET P1/P5 IF NECESSARY                *         
***********************************************************************         
GOPOWWOW NTR1  ,                                                                
         TM    16(R9),X'80'       TEST OVERRIDE UTL ENTRY PASSED                
         BZ    GOPOW2                                                           
         OI    DMCBW1,X'80'                                                     
         LA    RE,OVRDUTL                                                       
         ST    RE,DMCBW5                                                        
*                                                                               
GOPOW2   GOTO1 POWWOW,(R1)                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ CONTROL FILE                                        *         
***********************************************************************         
READ     NTR1  ,                                                                
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,IO,0                      
         CLI   DMCB+8,0                                                         
         BE    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* SET AND RESET SYSTEM TIMERS                                         *         
***********************************************************************         
SSET     LR    R0,RE                                                            
         GOTO1 ATICTOC,DUB,C'SSET' STOP TIMER                                   
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
RSET     LR    R0,RE                                                            
         GOTO1 ATICTOC,DUB,C'RSET' RESTART TIMER                                
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET ELEMENT IN CONTROL FILE RECORD                       *         
***********************************************************************         
                                                                                
         GETEL (R6),28,ELCODE                                                   
         EJECT                                                                  
*                                                                               
ARSOFF   SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         BR    RE                                                               
*                                                                               
XIT      XIT1  ,                                                                
         LTORG                                                                  
*                                                                               
ARZERO   DC    16F'0'                                                           
*                                                                               
ASOFTABL DC    A(SOFTABLE)                                                      
ADIRCARD DC    A(DIRCARD)                                                       
AARCCARD DC    A(ARCCARD)                                                       
ACOMSCRE DC    A(COMSCORE)                                                      
AMVSDSN  DC    A(MVSDSN)                                                        
NULLS    DC    XL20'00'                                                         
         EJECT                                                                  
***********************************************************************         
* PUT WRITE=NO                                                                  
***********************************************************************         
*&&US                                                                           
         USING RESTRICT,RF                                                      
         USING UTLD,R1                                                          
RESTRICT BASR  RF,0                                                             
         SHI   RF,2                                                             
         L     R1,AUTL                                                          
         TM    TTEST,TTESTURQ                                                   
         BZR   RE                                                               
         TM    FACFLG,SSBSYTST     IS THIS A TEST SYSTEM                        
         BO    REST10              DON'T ALLOW UPDATE SOONS                     
*                                                                               
RESTBASE CLC   =C'JT',TAGY                                                      
         JE    REST10              NO UPDATE SOON JOBS                          
         CLC   =C'GZ',TAGY                                                      
         JE    REST10              NO UPDATE SOON JOBS                          
         CLC   =C'S$',TAGY         KSL         S$                               
         JE    REST10                                                           
         CLC   =C'HH',TAGY         H&K         HH                               
         JE    REST10                                                           
         CLC   =C'HK',TAGY         H&K         HK                               
         JE    REST10                                                           
         CLC   =C'H8',TAGY         H&K         H8                               
         JE    REST10              ALLOW ALL BUT UPDATE SOONS                   
         CLC   =C'HL',TAGY         H&K         HL                               
*        BE    REST10              NEXT TEST                                    
*        CLC   =C'TH',TAGY         ZENITH      TH (TST SYSTEM)                  
         BNER  RE                                                               
REST10   MVI   WRITENO,YES         INCASE A RESET IS ISSUED                     
         BR    RE                                                               
         DROP  R1,RF                                                            
*                                                                               
         LTORG                                                                  
*&&                                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
*PRINT QUEUE FILE DATA AND MODEL JCL STATEMENTS                                 
***********************************************************************         
IDSPQS   DC    C'123456789ABCDEFG'                                              
*&&UK                                                                           
NUMPQS   DC    H'16'               UK HAS SIXTEEN PRTQ FILES                    
DQPQTAB  DC    AL1(0,0,0)          0                                            
         DC    AL1(1,0,1)          1 TST                                        
         DC    AL1(0,0,0)          2 AD1                                        
         DC    AL1(1,0,1)          3 TTS                                        
         DC    AL1(0,0,0)          4 AD2                                        
         DC    AL1(1,0,1)          5 NEW                                        
         DC    AL1(0,0,0)          6 AD3                                        
         DC    AL1(0,0,0)          7 ???                                        
         DC    AL1(0,0,0)          8 AD4                                        
         DC    AL1(1,0,1)          9 LP2                                        
         DC    AL1(0,0,0)         10 ADA                                        
         DC    AL1(2,0,2)         11 CSC                                        
         DC    AL1(0,0,0)         12 ADB                                        
         DC    AL1(3,0,3)         13 FQA                                        
         DC    AL1(0,0,0)         14 ADC                                        
         DC    AL1(0,0,0)         15                                            
*&&                                                                             
*&&US                                                                           
NUMPQS   DC    H'16'               US HAS SIXTEEN PRTQ FILES                    
DQPQTAB  DC    AL1(0,0,0)          0                                            
         DC    AL1(1,1,1)          1 TST                                        
         DC    AL1(0,0,0)          2 AD1                                        
         DC    AL1(0,0,0)          3 AD5                                        
         DC    AL1(0,0,0)          4 REPA                                       
         DC    AL1(0,0,0)          5 AD2                                        
         DC    AL1(1,1,1)          6 MEL                                        
         DC    AL1(0,0,0)          7 AD3                                        
         DC    AL1(0,0,0)          8 AD4                                        
         DC    AL1(0,0,0)          9 REPC                                       
         DC    AL1(0,0,0)         10 AD6                                        
         DC    AL1(2,2,2)         11 CSC                                        
         DC    AL1(0,0,0)         12 AD7                                        
         DC    AL1(0,0,0)         13 DARE                                       
         DC    AL1(0,0,0)         14 REPB                                       
         DC    AL1(3,3,3)         15 FQA                                        
*&&                                                                             
PRTQDD1  DC    CL80'//PRTQ1     DD  DSN=FAC.PRTQ10,DISP=SHR'                    
         DC    CL80'//PRTQ1     DD  DSN=CSC.PRTQ1,DISP=SHR'                     
         DC    CL80'//PRTQ1     DD  DSN=FQA.PRTQ1,DISP=SHR'                     
*                                                                               
QDQDD1   DC    CL80'//DDSQDQ    DD  DSN=FAC.DDSQDQ0,DISP=SHR'                   
         DC    CL80'//DDSQDQ    DD  DSN=CSC.DDSQDQ,DISP=SHR'                    
         DC    CL80'//DDSQDQ    DD  DSN=FQA.DDSQDQ,DISP=SHR'                    
*                                                                               
QDCDD1   DC    CL80'//DDSQDC    DD  DSN=FAC.DDSQDC0,DISP=SHR'                   
         DC    CL80'//DDSQDC    DD  DSN=CSC.DDSQDC,DISP=SHR'                    
         DC    CL80'//DDSQDC    DD  DSN=FQA.DDSQDC,DISP=SHR'                    
*                   0.........1.........2.........3.........4.........          
*                   01234567890123456789012345678901234567890123456789          
*                                                                               
MVSJOB   DC    CL80'//JJJJJJJJ JOB UUUUS0PP1,,MSGLEVEL=(1,1),'                  
*&&UK                                                                           
MVSJOB1  DC    CL80'//             MSGCLASS=Y,CLASS=A,PRTY=5,USER=SOON,*        
               PASSWORD=JUNE1991'                                               
*&&                                                                             
*&&US                                                                           
MVSJOB1  DC    CL80'//             MSGCLASS=Y,CLASS=A,PRTY=5'                   
*&&                                                                             
MVSLST   DC    CL80'//SYSPRINT  DD SYSOUT=*'                                    
*                                                                               
JESMAIN  DC    CL80'//*MAIN CLASS='                                             
*                   0123456789012345678901234567890123456789                    
DIRCARD  DC    CL40'DIRECT=SPP,1S  ,IIIII,       , , ,     '                    
         DC    CL40'               ,           ,           '                    
*                012345678901234567890123456789012345678901234567890            
ARCCARD  DC    C'ARC=AA,SPP,1S    ,DDDDD,UUUUU, ,C,00000000        '            
         DC    C'                              '                                
*                                                                               
COMSCORE DC    CL80'COMSCORE=1'                                                 
*                                                                               
MVSDSN   DC    CL80'MVSDSN='                                                    
         ORG   *-2                                                              
         DC    C'+'                                                             
         EJECT                                                                  
*SUBSIDIARY SCAN TABLE FOR SOFT JCL STATEMENTS                                  
*                                                                               
SOFTABLE DS    0XL20                                                            
         DC    AL1(7,2),AL3(SETAGY),CL15'&&AGENCY'                              
         DC    AL1(8,1),AL3(SETCMP),CL15'&&COMPANY'                             
         DC    AL1(7,1),AL3(SETLDG),CL15'&&LEDGER'                              
         DC    AL1(4,2),AL3(SETREP),CL15'&&REP'                                 
         DC    AL1(4,8),AL3(SETDAT),CL15'&&IPL'                                 
         DC    AL1(9,8),AL3(SETBIL),CL15'&&BILLDATE'                            
         DC    AL1(8,2),AL3(SETPRG),CL15'&&PROGRAM'                             
         DC    AL1(7,2),AL3(SETSYS),CL15'&&SYSTEM'                              
         DC    AL1(6,7),AL3(SETLG1),CL15'&&LOGO1'                               
         DC    AL1(6,7),AL3(SETLG2),CL15'&&LOGO2'                               
         DC    AL1(7,6),AL3(SETORG),CL15'&&ORIGIN'                              
         DC    AL1(4,2),AL3(SETDAY),CL15'&&DAY'                                 
         DC    AL1(8,2),AL3(SETHEX),CL15'&&HEXCOMP'                             
         DC    AL1(7,1),AL3(SETFAC),CL15'&&FACPAK'                              
         DC    AL1(0)                                                           
EQUJOB   EQU   X'81'               EQUATES FOR JCLTYPE CARD TYPES               
EQUEXEC  EQU   X'82'                                                            
EQUDD    EQU   X'84'                                                            
EQUEOJ   EQU   X'88'                                                            
EQUCOMM  EQU   X'90'                                                            
EQUJFORM EQU   X'41'                                                            
EQUJMAIN EQU   X'42'                                                            
EQUJNET  EQU   X'44'                                                            
         EJECT                                                                  
***********************************************************************         
* TWA OVERRIDES                                                       *         
***********************************************************************         
TWAD     DSECT                                                                  
         DS    XL10                                                             
ID       DS    XL2                                                              
         DS    XL2                                                              
AGENCY   DS    CL2                                                              
ONOFF    DS    XL1                                                              
DEST     DS    XL2                                                              
OUTPUT   DS    CL6                                                              
WHEN     DS    XL1                                                              
         DS    XL6                                                              
PRINT    DS    V                                                                
TWASCR   DS    XL1                                                              
         DS    CL3                                                              
TWAVBUFF DS    V                                                                
TWAVSORT DS    V                                                                
TWAPTCHR DS    V                                                                
TWAVWORK DS    V                                                                
TWADIRCT DS    CL3                 SAVED REQSTR ID AS SPOOL ID                  
         EJECT                                                                  
***********************************************************************         
* DDSPOOK                                                             *         
***********************************************************************         
       ++INCLUDE DDSPOOK                                                        
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
K        EQU   1024                                                             
                                                                                
SPOOND   DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
FULL2    DS    F                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
*                                                                               
DMCB     DS    8F                                                               
         ORG   DMCB                                                             
DMCBW1   DS    A                                                                
DMCBW2   DS    A                                                                
DMCBW3   DS    A                                                                
DMCBW4   DS    A                                                                
DMCBW5   DS    A                                                                
DMCBW6   DS    A                                                                
DMCBW7   DS    A                                                                
DMCBW8   DS    A                                                                
*                                                                               
RELO     DS    A                                                                
DATAMGR  DS    V                                                                
GETBOOK  DS    V                                                                
HEXOUT   DS    V                                                                
REPLACE  DS    V                                                                
POWWOW   DS    V                                                                
SWITCH   DS    V                                                                
PQPROF   DS    V                                                                
LOCKSPC  DS    V                                                                
GETFACT  DS    V                                                                
DATCON   DS    V                                                                
PROTON   DS    V                                                                
PROTOFF  DS    V                                                                
ASYSFAC  DS    A                                                                
ATICTOC  DS    A                                                                
AUTL     DS    A                                                                
ASSB     DS    A                                                                
TBLET    DS    A                                                                
*                                                                               
WORK     DS    XL32                                                             
JOBHDRL  DS    H                                                                
JOBTABL  DS    H                                                                
*                                                                               
ACLFRST  DS    A                   A(FIRST IN CLASS TABLE)                      
ACLLAST  DS    A                   A(END OF CLASS TABLE - 1)                    
AJTFRST  DS    A                   A(FIRST IN JOB TABLE)                        
AJTLAST  DS    A                   A(END OF JOB TABLE - 1)                      
*                                                                               
ASPSV    DS    F                   A(SPOOL SAVE AREA FOR IRDR)                  
SPSV     DS    0XL14               COPY OF SPOOL SAVE AREA                      
SPSVTCNT DS    X                   TOTAL CARD COUNT                             
SPSVXCNT DS    X                   EXEC CARD COUNT                              
SPSVJID  DS    CL4                 JOB ID                                       
SPSVJCLA DS    C                   JOB CLASS                                    
SPSVJPRI DS    C                   JOB PRIORITY                                 
SPSVPCLA DS    C                   PRINT CLASS                                  
SPSVPFNO DS    CL4                 PRINT FNO                                    
         DS    X                   SPARE                                        
*                                                                               
DSPACE   DS    CL1                 DSPACE FROM GETFACT                          
FACID    DS    CL1                 FACPAK ONE CHARACTER ID                      
FACNAM   DS    CL3                 FACPAK 3 CHARACTER ID                        
FACFLG   DS    CL1                 FACPAK FLAGS                                 
REQCON   DS    CL1                 REQUEST CONTROL BYTE                         
JOBCLA   DS    CL1                 JOB CLASS                                    
JOBPRI   DS    CL1                 JOB PRIORITY                                 
PRTCLA   DS    CL1                 SYSPRINT CLASS                               
PRTFNO   DS    CL4                 SYSPRINT FORMS                               
PRTQ     DS    CL1                 PRINT QUEUE DD STATEMENTS                    
*                                                                               
JCLNAME  DS    CL8                 JCL CARD NAME //XXXXXXXX .........           
JCLACTN  DS    CL8                 JCL CARD ACTN //........ XXXX ....           
JCLTYPE  DS    X                   JCL CARD TYPE                                
JCLSTYPE DS    X                   JCL CARD SUB TYPE                            
JCLOPRD  DS    XL2                 JCL CARD DISP TO FIRST OPERAND               
*                                                                               
JESCODE  DS    CL4                                                              
JESJOB   DS    CL8                                                              
*                                                                               
JESKEY   DS    0XL32                                                            
JESKUID  DS    XL2                 USER ID                                      
JESKDID  DS    CL3                 REPORT ID                                    
JESKCLA  DS    CL1                 REPORT CLASS                                 
JESKREPN DS    XL2                 REPORT NUMBER                                
JESKRETL DS    XL2                 RETAIN HOURS LIVE                            
JESKRETD DS    XL2                 RETAIN HOURS DEAD                            
JESKTYPE DS    XL1                 REPORT TYPE FLAGS                            
JESKRET  DS    CL1                 REPORT RETAIN CLASS                          
JESKSUB  DS    CL2                 REPORT SUBID (PQCLASS)                       
JESKCPY  DS    XL1                 REPORT NUMBER OF COPIES                      
JESKSTA  DS    XL1                 REPORT STATUS                                
JESKRTY  DS    CL1                 REPORT TYPE                                  
JESKARC  DS    CL1                 REPORT ARCHIVE CLASS                         
JESKADT  DS    XL4                 REPORT ARCHIVE DOCUMENT TYPE                 
JESKTYP1 DS    XL1                 REPORT ARCHIVE TYPE                          
JESKPSWD DS    CL4                 REPORT PID/PIN                               
JESKSEC1 DS    XL1                 REPORT SECURITY FLAGS#1                      
JESKSEC2 DS    XL1                 REPORT SECURITY FLAGS#2                      
         DS    CL2                 SPARE                                        
*                                                                               
JESCARD  DS    CL80                                                             
*                                                                               
TODAYP   DS    XL3                 DATE PACKED                                  
TODAYB   DS    XL3                 DATE BINARY                                  
TODAY    DS    CL8                 DATE C'DD/MM/YY' OR C'MM/DD/YY'              
BILLDATE DS    CL8                 BILLING DATE C'DD/MM/YY' C'MM/DD/YY'         
BOOK     DS    CL10                                                             
*                                                                               
DQDDN    DS    XL1                 DDSQDQ FILE DD STATEMENT NUMBER              
DCDDN    DS    XL1                 DDSQDC FILE DD STATEMENT NUMBER              
PQDDN    DS    XL1                 PRTQ FILE DD STATEMENT NUMBER                
*                                                                               
RFHDATA  DS    CL80                                                             
RFHCARD  DS    CL80                                                             
MAINCARD DS    CL80                                                             
CARD     DS    CL80                                                             
REQ      DS    CL80                                                             
PROFKEY  DS    CL5                                                              
KEY      DS    CL32                                                             
SPACES   DS    CL110                                                            
ELCODE   DS    CL1                                                              
IF       DS    CL1                                                              
MOREJCL  DS    CL1                                                              
EXEC     DS    CL1                                                              
MAINCLAS DS    CL3                                                              
FILSYS   DS    XL1                 SYSTEM NUMBER (1-15)                         
FILSEN   DS    XL1                 SYSTEM SE NUMBER (1-255)                     
SYSNID   DS    CL2                 SYSTEM NAME ID                               
SYSID    DS    CL1                 FACFAC SYSTEM ID NUMBER                      
WRITENO  DS    CL1                 WRITE=NO                                     
QTYPE    DS    X                   QUEUE TYPE - S/M/L                           
ORIGNADD DS    CL66                                                             
LOGOS    DS    CL14                                                             
MYREMOTE DS    XL80                AREA TO BUILD REMOTED                        
OKREMOTE DS    X                   FLAG TO SAY IF MYREMOTE HAS DATA             
*                                  X'01' PROFILE FOUND                          
*                                  X'02' ARC= CARD                              
*                                  X'04' DON'T ARC                              
REPORTYP DS    C                   REPORT TYPE                                  
ARCCLASS DS    C                   ARCHIVE CLASS                                
ARCTY1   DS    X                   ARCHIVE TYPE X'80'=ARCE,X'40'=ARCA           
ARCDOCTY DS    XL4                 ARCHIVE DOCUMENT TYPE                        
RUNJOB   DS    C                   YES/NO                                       
*                                                                               
AREQCRDS DS    A                   A(REQUEST CARDS FROM PARAMETER LIST)         
SOFTNTRY DS    F                                                                
OVRDUTL  DS    XL256                                                            
SOFTWORK DS    20XL32                                                           
IO       DS    (2*K)C                                                           
BOOKBUFF DS    (2*K)C                                                           
SPOONDL  EQU   *-SPOOND                                                         
         EJECT ,                                                                
**********************************************************************          
* OTHER DSECTS                                                                  
**********************************************************************          
SPTABD   DSECT                     SPECIALS FOR SPECIFIC SYSTEM                 
SPPROG   DS    CL2                 PROGRAM ID (UUUUS0PP1)                       
SPSUBSYS DS    CL1                 SUB SYSTEM ID                                
SPTABLQ  EQU   *-SPTABD                                                         
EOTQ     EQU   X'FF'                                                            
         EJECT                                                                  
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
* FATABSJOB                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSJOB                                                      
         PRINT ON                                                               
* FATABSDEQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE FATABSDEQU                                                     
         PRINT ON                                                               
* DMDDNAMED                                                                     
DDNAMED  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DMDDNAMED                                                      
         PRINT ON                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DMREQHDRA                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMREQHDRA                                                      
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043FASPOON   04/26/17'                                      
         END                                                                    
