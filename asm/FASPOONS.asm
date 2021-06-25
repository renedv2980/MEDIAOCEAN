*          DATA SET FASPOONS   AT LEVEL 004 AS OF 05/01/02                      
*PHASE T00A25A                                                                  
*INCLUDE GETBOOK                                                                
*INCLUDE REPLACE                                                                
         SPACE 2                                                                
*PARAM1  XL1   X'FF'=RETURN A(REPORT KEY) IN PARAM6                             
*              X'00' RETURNED IF OK                                             
*              X'01' RETURNED IF JCL BOOK NOT FOUND                             
*              X'03' RETURNED IF JOB TABLE IS FULL                              
*        AL3   A(TWA)                                                           
*PARAM2  XL1   REQUEST CARD CONTROL VALUE                                       
*        AL3   A(REQUEST CARD)                                                  
*PARAM3  XL1   EXTENDED MULTIPLE REQUEST CARD NUMBER (PARAM2=X'FE')             
*        AL3   A(COMFACS)                                                       
*PARAM4  XL1   N/D                                                              
*        AL3   A(JCLBOOK)                                                       
*PARAM5  XL1   N/D                                                              
*        AL3   A(SPOOK)                                                         
*PARAM6  XL1   N/D                                                              
*        AL3   A(REPORT KEY) RETURNED IF P1 BYTE 0 SET                          
         TITLE 'FASPOON - ON LINE JOB SPOOLER/SCHEDULER '                       
FASPOON  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 SPOONDL,**SPOON*,R7,RR=RE,CLEAR=YES                              
         USING SPOOND,RC                                                        
         ST    RE,RELO                                                          
         XC    ASPSV,ASPSV                                                      
         XC    SPSV,SPSV           CLEAR SPOOL SAVE AREA                        
*                                                                               
         LR    R9,R1                                                            
         L     RA,0(R9)            RA=A(TWA)                                    
         USING TWAD,RA                                                          
         MVC   REQCON,4(R9)        SAVE REQUEST CONTROL BYTE                    
         L     R1,4(R9)            SAVE REQUEST CARD                            
         MVC   REQ,0(R1)                                                        
         ST    R1,AREQCRDS         SAVE A(REQUEST CARD)                         
         L     R1,12(R9)                                                        
         MVC   BOOK,0(R1)          SAVE JCL BOOK NAME                           
*                                                                               
         CLC   BOOK(2),=C'NE'      TEST BUILT NETWORK BOOK NAME                 
         BNE   *+10                                                             
         MVC   BOOK(2),=C'SP'      FORCE TO SPOT BOOK                           
*                                                                               
         L     R8,16(R9)                                                        
         USING SPOOK,R8            R8=A(SPOOK)                                  
*&&UK                                                                           
MEFIX    CLI   SPOOKSYS,C'M'       ENSURE MEDLINE IS C'ME'                      
         BNE   MEFIXX                                                           
         CLI   SPOOKSYS+1,C'B'     MB FOR MEDIABASE                             
         BE    MEFIXX                                                           
         CLI   SPOOKSYS+1,C'P'     MP FOR MEDIA PLANNING                        
         BE    MEFIXX                                                           
         MVI   SPOOKSYS+1,C'E'                                                  
MEFIXX   EQU   *                                                                
*&&                                                                             
         L     RF,8(R9)            RF=A(COMFACS)                                
         USING COMFACSD,RF                                                      
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   HEXOUT,CHEXOUT                                                   
         MVC   SWITCH,CSWITCH                                                   
         MVC   PQPROF,CPQPROF                                                   
         MVC   PROTON,CPROTON                                                   
         MVC   PROTOFF,CPROTOFF                                                 
         L     RF,CGETFACT                                                      
*                                                                               
         GOTO1 (RF),DMCB,0         A(FACPAK EXTRA DATA)                         
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         MVC   TODAY,FADATE                                                     
         MVC   POWWOW,FAPOWWOW                                                  
         MVC   SYSID,FASYSID       SAVE FACPAK SYSTEM ID NUMBER                 
         MVC   FILSYS,FAOVSYS      SAVE SYSTEM NUMBER                           
         MVC   FILSEN,FASYS        SAVE SYSTEM SE NUMBER                        
         MVC   FILSET,FAFILSET     SAVE SYSTEM FILESET NUMBER                   
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
         L     RF,VSSB-SYSFACD(RF)                                              
         MVC   FACID,SSBSYSN1-SSBD(RF)                                          
         GOTO1 SWITCH,DMCB,X'FFFFFFFF'                                          
         L     RF,DMCB                                                          
         ST    RF,AUTL             SAVE A(UTL ENTRY)                            
         EJECT                                                                  
*REQCON SELECTS ROUTINE                                                         
*                                                                               
CTRL     NC    SPOOKUID,SPOOKUID                                                
         BNZ   *+10                                                             
         MVC   SPOOKUID,ID                                                      
         NC    SPOOKEOD,SPOOKEOD                                                
         BNZ   *+10                                                             
         MVC   SPOOKEOD,REQ                                                     
         NC    SPOOKAGY,SPOOKAGY                                                
         BNZ   *+10                                                             
         MVC   SPOOKAGY,AGENCY                                                  
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
BIGREQ   BAS   RE,BEFORE           X'1N'=BIG RQST CARD (N=#CRD ENTRIES)         
         BNE   EXIT                JOB TABLE IS FULL                            
         BAS   RE,GETJCL                                                        
         BNE   EXIT                                                             
         BAS   RE,RFHDR            OUTPUT REQ HDR CARD IF REQUIRED              
         ZIC   R0,REQCON           PICK UP NUMBER OF CARDS                      
         SLL   R0,32-4                                                          
         SRL   R0,32-4                                                          
         L     R1,AREQCRDS         POINT TO FIRST CARD IN RECORD                
BIGREQ2  MVC   CARD,0(R1)                                                       
         BAS   RE,PUT                                                           
         LA    R1,80(,R1)                                                       
         BCT   R0,BIGREQ2                                                       
         BAS   RE,LASTJCL                                                       
         BAS   RE,AFTER                                                         
         B     EXIT                                                             
*                                                                               
EXTREQ   EQU   *                   EXTENDED MULTIPLE REQUEST CARDS              
         BAS   RE,BEFORE           NUMBER OF CARDS IN PARAM3                    
         BNE   EXIT                JOB TABLE IS FULL                            
         BAS   RE,GETJCL                                                        
         BNE   EXIT                                                             
         BAS   RE,RFHDR            OUTPUT REQ HDR CARD IF REQUIRED              
         ZIC   R0,8(R9)            PICK UP NUMBER OF CARDS IN PARAM3            
         LTR   R0,R0                                                            
         BZ    EXTREQ4                                                          
         L     R1,AREQCRDS         POINT TO FIRST CARD IN RECORD                
EXTREQ2  MVC   CARD,0(R1)                                                       
         BAS   RE,PUT                                                           
         LA    R1,80(,R1)                                                       
         BCT   R0,EXTREQ2                                                       
EXTREQ4  BAS   RE,LASTJCL                                                       
         BAS   RE,AFTER                                                         
         B     EXIT                                                             
*                                                                               
ALL      BAS   RE,BEFORE           X'00'=NO CARDS                               
         BNE   EXIT                JOB TABLE IS FULL                            
         BAS   RE,GETJCL                                                        
         BNE   EXIT                                                             
         BAS   RE,AFTER                                                         
         B     EXIT                                                             
*                                                                               
FIRST    BAS   RE,BEFORE           X'01'=FIRST CARD                             
         BNE   EXIT                JOB TABLE IS FULL                            
         BAS   RE,GETJCL                                                        
         BNE   EXIT                                                             
         BAS   RE,RFHDR            OUTPUT REQ HDR CARD IF REQUIRED              
*                                                                               
SUBS     MVC   CARD,REQ            X'02'=SUBSEQUENT CARDS                       
         BAS   RE,PUT                                                           
         B     EXIT                                                             
*                                                                               
ONLY     BAS   RE,BEFORE           X'03'=ONE DATE CARD ONLY                     
         BNE   EXIT                JOB TABLE IS FULL                            
         BAS   RE,GETJCL                                                        
         BNE   EXIT                                                             
         BAS   RE,RFHDR            OUTPUT REQ HDR CARD IF REQUIRED              
*                                                                               
LAST     MVC   CARD,REQ            X'FF'=LAST CARD                              
         BAS   RE,PUT                                                           
         BAS   RE,LASTJCL                                                       
         BAS   RE,AFTER                                                         
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
         CLC   =C'TSO',JESKDID     IF SUBID IS TSO. . .                         
         BNE   XIT                                                              
         L     RF,AUTL             . . . AND IT'S A DDS TERMINAL. . .           
         TM    TSTAT1-UTLD(RF),TSTATDDS                                         
         BZ    XIT                 . . . THEN IT'S NOT SUBMITTED                
*                                                                               
FREENTRY L     RF,ASYSFAC                                                       
         L     RF,VSSB-SYSFACD(RF)                                              
         L     RF,SSBTKADR-SSBD(RF)                                             
         ICM   R2,15,TCBSJENT-TCBD(RF)                                          
         BZ    XIT                                                              
         GOTO1 PROTOFF                                                          
         MVI   JOBSTAT-JOBTABD(R2),JOBSAVA  RELEASE JOB ENTRY                   
         GOTO1 PROTON                                                           
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*INITIAL - ID DETAILS                                                           
*                                                                               
BEFORE   NTR1                                                                   
         L     RF,ASYSFAC          TEST IF MONSOON SUPPORT                      
         L     RF,VSSB-SYSFACD(RF)                                              
         LA    RE,SSBJESIO-SSBD(RF)                                             
         CLI   0(RE),C' '          IS THIS APPLICATION RUNNING MONSOON?         
         BE    *+12                YES                                          
         CLI   SPOOKWEN,6          SOON FOR LUNATIC?                            
         BNE   BEF1                                                             
*                                                                               
         CLI   SPOOKWEN,7          SPECIAL OVERNIGHT SOON                       
         BE    BEF1                                                             
*                                                                               
         BAS   RE,FINDNTRY         YES GRAB AN EMPTY JOB TABLE ENTRY            
         L     RF,SSBTKADR-SSBD(RF)                                             
         ICM   RE,15,TCBSJENT-TCBD(RF)                                          
         BNZ   BEF1                                                             
         DC    H'0',C'$JTFULL '    TABLE IS FULL - UNWIND WITH ERROR            
*                                                                               
BEF1     XC    KEY,KEY             GET ID RECORD                                
         LA    R4,KEY                                                           
         USING CTIREC,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),SPOOKUID                                             
         BAS   RE,READ                                                          
*                                                                               
BEF2     MVC   ORIGNADD,SPACES     ORIGIN DETAILS                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'36'                                                     
         BAS   RE,GETEL                                                         
         BNE   BEF3                                                             
         MVC   ORIGNADD,2(R6)                                                   
*                                                                               
BEF3     NC    SPOOKDES,SPOOKDES   DESTINATION DETAILS                          
         BZ    BEF3A                                                            
         CLC   SPOOKDES,SPOOKUID                                                
         BE    BEF3A                                                            
         XC    KEY,KEY             GET DEST ID RECORD                           
         LA    R4,KEY                                                           
         USING CTIREC,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),SPOOKDES                                             
         BAS   RE,READ                                                          
BEF3A    LA    R6,IO                                                            
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTDSTD,R6                                                        
         MVC   LOGOS,CTDSTLG1                                                   
         MVC   JESCODE,CTDSTPOW                                                 
         NC    SPOOKDID,SPOOKDID                                                
         BNZ   BEF4                                                             
         MVC   SPOOKDID,TWADIRCT                                                
*&&UK                                                                           
         CLC   SPOOKSYS,=C'ME'                                                  
         BNE   *+10                                                             
         MVC   SPOOKDID,JESJOB+4                                                
*&&                                                                             
BEF4     MVI   JOBCLA,C'A'         SET DEFAULT JOB CLASS                        
         MVI   JOBPRI,C'5'         SET DEFAULT JOB PRIORITY                     
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
         BNE   BEF4A                                                            
         MVI   JESJOB+3,C'X'                                                    
         CLI   JESJOB+2,C' '       ALSO FOR 2 CHR                               
         BNE   BEF4A                                                            
         MVI   JESJOB+2,C'X'                                                    
BEF4A    MVC   JESCARD,SPACES                                                   
         CLI   SPOOKWEN,8                                                       
         BNE   BEF5                                                             
         MVC   CARD,=CL80'MQ EXTERNAL JOB REQUEST PENDING'                      
         BAS   RE,PUT              WRITE DUMMY JOB CARD                         
         B     BEF6                                                             
*                                                                               
BEF5     MVC   CARD,MVSJOB         SET FIRST MVS JOB CARD                       
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
         BNE   BEF5B                                                            
         LA    R1,MESPTAB          SEARCH MEDIA SYS SPECIALS                    
         USING SPTABD,R1                                                        
*                                                                               
BEF5A    CLI   0(R1),EOTQ                                                       
         BE    BEF5B                                                            
         CLC   SPPROG,6(RE)        TEST IF PROGRAM PP IN LIST                   
         BE    *+12                                                             
         LA    R1,SPTABLQ(R1)                                                   
         B     BEF5A                                                            
         MVC   5(1,RE),SPSUBSYS    SET SUBSYSTEM CHARACTER                      
         B     BEF5B                                                            
MESPTAB  DC    C'CR1CT1FC1FG1FQ1FT1NC1NG1NT1YQ1' CRAFT REQUESTS                 
         DC    AL1(EOTQ)                                                        
         DROP  R1                                                               
BEF5B    DS    0H                                                               
*&&                                                                             
         BAS   RE,PUT              WRITE FIRST JOB CARD                         
*                                                                               
BEF6     MVC   CARD,MVSJOB1        SET SECOND MVS JOB CARD                      
*&&US*&& MVI   CARD+24,C'0'        MSGCLASS=0                                   
         OC    SPOOKPR1(2),SPOOKPR1                                             
         BNZ   BEF6N               CALLER PASSED SPOOK WITH JOB INFO            
*                                                                               
*&&UK                                                                           
         CLC   SPOOKSYS,=C'ME'     SET MEDIA SYSTEM DEFAULTS                    
         BNE   BEF6B                                                            
         MVI   JOBCLA,C'M'         MEDIA SYSTEM CLASS/PRTY                      
         MVI   JOBPRI,C'5'                                                      
         B     BEF7                                                             
*&&                                                                             
*                                                                               
BEF6B    CLI   SPOOKSYS,C'A'       SET ACCOUNT SYSTEM DEFAULTS                  
         BNE   BEF6C                                                            
         MVI   JOBCLA,C'A'         ACCOUNT SYSTEM CLASS/PRTY                    
         MVI   JOBPRI,C'5'                                                      
         CLI   REQCON,3            AS LONG AS IT'S A 1 CARD REQUEST             
         BE    BEF7                                                             
         CLI   REQCON,X'11'        AS ABOVE                                     
         BE    BEF7                                                             
*&&UK*&& MVI   JOBPRI,C'5'                                                      
*&&US*&& MVI   JOBPRI,C'5'                                                      
         B     BEF7                                                             
*                                                                               
BEF6C    B     BEF7                                                             
*                                                                               
BEF6N    CLI   SPOOKPR1,C'A'       TEST VALID SPOOK CLASS                       
         BL    BEF6P                                                            
         CLI   SPOOKPR1,C'9'                                                    
         BH    BEF6P                                                            
         MVC   JOBCLA,SPOOKPR1     SET JOB CLASS FROM SPOOK                     
*                                                                               
BEF6P    CLI   SPOOKPR2,C'0'       TEST VALID SPOOK PRIORITY                    
         BL    BEF7                                                             
         CLI   SPOOKPR2,C'9'                                                    
         BH    BEF7                                                             
         MVC   JOBPRI,SPOOKPR2     SET JOB PRIORITY FROM SPOOK                  
*                                                                               
BEF7     DS    0H                                                               
         CLI   SPOOKWEN,8          TEST MQIO EXTERNAL JOB REQUEST               
         BE    BEF8X               IF SO DONT PUT JOB CARDS                     
         MVC   CARD+32(1),JOBCLA   SET CLASS/PRTY AND RELEASE                   
         MVC   CARD+39(1),JOBPRI                                                
*&&UK                                                                           
         B     BEF7B         <---  REMOVE THIS TO RUN MONSOON A.M.P             
         CLI   JOBCLA,C'A'         TYPRUN=SCAN FOR CLASS A.M.P                  
         BE    BEF7A                                                            
         CLI   JOBCLA,C'M'                                                      
         BE    BEF7A                                                            
         CLI   JOBCLA,C'P'                                                      
         BE    BEF7A                                                            
         B     BEF7B                                                            
BEF7A    CLC   SPOOKUID,=X'007C'   ID=JWAL                                      
         BNE   *+14                                                             
         CLC   SPOOKDID,=C'DDS'                                                 
         BE    *+10                                                             
         MVC   CARD+40(12),=C',TYPRUN=SCAN'                                     
*&&                                                                             
BEF7B    BAS   RE,PUT                                                           
*                                                                               
BEF8     MVC   MAINCARD,SPACES     INITIALISE DEFAULT JES MAIN CARD             
         MVC   MAINCLAS,SPOOKSYS                                                
         SR    RE,RE                                                            
         ICM   RE,1,FILSET                                                      
         BZ    *+14                                                             
         LA    RE,FILLET(RE)                                                    
         MVC   MAINCLAS+1(1),0(RE)                                              
*&&UK*&& B     BEF8X                                                            
*&&US                                                                           
BEF8A    CLI   SPOOKSYS,C'A'       ACCOUNT                                      
         BE    BEF8T                                                            
BEF8B    CLI   SPOOKSYS,C'N'       NETWORK                                      
         BNE   BEF8C                                                            
         MVI   MAINCLAS,C'S'       SWITCH NETWORK INTO SPOT CLASS               
         CLI   MAINCLAS+1,C'1'     GUESS WHAT SPOT?=NET?                        
         BNE   *+12                                                             
         MVI   MAINCLAS+1,C'8'                                                  
         B     BEF8T                                                            
         CLI   MAINCLAS+1,C'2'                                                  
         BNE   *+12                                                             
         MVI   MAINCLAS+1,C'9'                                                  
         B     BEF8T                                                            
         CLI   MAINCLAS+1,C'3'                                                  
         BNE   *+12                                                             
         MVI   MAINCLAS+1,C'A'                                                  
         B     BEF8T                                                            
         CLI   MAINCLAS+1,C'4'                                                  
         BNE   *+12                                                             
         MVI   MAINCLAS+1,C'C'                                                  
         B     BEF8T                                                            
         CLI   MAINCLAS+1,C'5'                                                  
         BNE   *+12                                                             
         MVI   MAINCLAS+1,C'J'                                                  
         B     BEF8T                                                            
         CLI   MAINCLAS+1,C'6'                                                  
         BNE   *+12                                                             
         MVI   MAINCLAS+1,C'K'                                                  
         B     BEF8T                                                            
         CLI   MAINCLAS+1,C'W'                                                  
         BNE   *+12                                                             
         MVI   MAINCLAS+1,C'W'                                                  
         B     BEF8T                                                            
BEF8C    CLI   SPOOKSYS,C'S'       SPOT                                         
         BE    BEF8T                                                            
BEF8D    CLI   SPOOKSYS,C'P'       PRINT                                        
         BE    BEF8T                                                            
BEF8E    CLI   SPOOKSYS,C'R'       REP                                          
         BE    BEF8T                                                            
BEF8F    CLI   SPOOKSYS,C'B'       MEDIA BASE                                   
         BE    BEF8T                                                            
BEF8G    CLI   SPOOKSYS,C'C'       CONTROL                                      
         BNE   BEF8H                                                            
         MVI   MAINCLAS+1,C'1'                                                  
         B     BEF8T                                                            
BEF8H    CLI   SPOOKSYS,C'D'       CPP                                          
         BE    BEF8T                                                            
BEF8I    CLI   SPOOKSYS,C'T'       TALENT                                       
         BE    BEF8T                                                            
BEF8J    CLI   SPOOKSYS,C'M'       MEDIA PLANNING                               
         BNE   BEF8X                                                            
         MVI   MAINCLAS,C'L'                                                    
*                                                                               
BEF8T    MVC   MAINCARD,JESMAIN    //*MAIN CLASS=....                           
         MVC   MAINCARD+14(2),MAINCLAS                                          
         MVC   MAINCARD+16(4),=C'SOON'                                          
         B     BEF8W                                                            
*&&                                                                             
BEF8W    MVC   CARD,MAINCARD                                                    
         BAS   RE,PUT                                                           
*                                                                               
BEF8X    BAS   RE,SETSOFT                                                       
*                                                                               
         L     RF,ASYSFAC           A(SYSFACS)                                  
         L     RF,VSSB-SYSFACD(RF)  A(SSB)                                      
         LA    RE,SSBJESIO-SSBD(RF)                                             
         CLI   0(RE),C' '          IS THIS APPLICATION RUNNING MONSOON?         
         BE    *+12                YES                                          
         CLI   SPOOKWEN,6          SOON FOR LUNATIC?                            
         BNE   BEF8Y                                                            
*                                                                               
         CLI   SPOOKWEN,7          SPECIAL OVERNIGHT SOON                       
         BE    BEF8Y                                                            
*                                                                               
         L     RF,SSBTKADR-SSBD(RF) A(TCB ENTRY)                                
         L     R2,TCBSJENT-TCBD(RF) A(THIS JOB TABLE ENTRY)                     
         GOTO1 PROTOFF                                                          
         MVC   JOBMVSID-JOBTABD(,R2),JESJOB     SAVE MVS JOBNAME                
         GOTO1 PROTON                                                           
*                                                                               
BEF8Y    CR    RE,RE               SET CC EQUAL                                 
*                                                                               
BEFXIT   B     XIT                                                              
         SPACE 2                                                                
AFTER    NTR1                                                                   
*                                                                               
         CLI   MOREJCL,C'Y'                                                     
         BNE   *+8                                                              
         BAS   RE,GETJCL                                                        
         GOTO1 POWWOW,DMCB,=C'SCH',=C'EOJ',JESKEY,JESCARD                       
*                                                                               
         L     RF,ASYSFAC                                                       
         L     RF,VSSB-SYSFACD(RF)                                              
         LA    RE,SSBJESIO-SSBD(RF)                                             
         CLI   0(RE),C' '          IS THIS APPLICATION RUNNING MONSOON?         
         BE    *+12                YES                                          
         CLI   SPOOKWEN,6          SOON FOR LUNATIC?                            
         BNE   AFTERX                                                           
*                                                                               
         CLI   SPOOKWEN,7          SPECIAL OVERNIGHT SOON                       
         BE    AFTERX                                                           
*                                                                               
         BAS   RE,FILLNTRY         YES FILL IN JOB TABLE ENTRY                  
*                                                                               
AFTERX   B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO CREATE RFHDR= CARD THAT PRECEEDS EACH REQUEST                      
*                                                                               
RFHDR    NTR1                                                                   
         LA    R4,RFHDATA          R4=A(REQ FILE HEADER DATA)                   
         USING RQHHDR,R4                                                        
         XC    RQHHDR,RQHHDR                                                    
         MVC   RQHSYSID,SYSID                                                   
*                                                                               
RFHDR1   L     R3,AUTL             R3=A(UTL ENTRY)                              
         USING UTLD,R3                                                          
         MVC   RQHSIN,TSIN+1       MOVE UTL DATA TO HEADER                      
         MVC   RQHLINE(8),TSYM                                                  
         MVC   RQHACCS,TACCS                                                    
         MVC   RQHPSWD,TPASSWD                                                  
         MVC   RQHSYS,TSYS                                                      
         MVC   RQHOVSYS,TOVSYS                                                  
         MVC   RQHPRG,TPRG                                                      
         MVC   RQHCTRY,TCTRY                                                    
         MVC   RQHLANG,TLANG                                                    
         MVC   RQHAGCTY,TAGCTRY                                                 
         MVC   RQHAGCUR,TAGCURR                                                 
         MVC   RQHOFF,TOFFCODE     OFFICE NUMBER                                
         MVI   RQHCTRL,0                                                        
         MVC   RQHAGY,TAGY         AGENCY ALPHA                                 
         MVC   RQHORIG,TUSER       MOVE ORIGINATING USER ID NUM                 
*                                                                               
RFHDR2   MVC   RFHCARD,SPACES      BUILD RFHDR= CARD                            
         MVC   RFHCARD(6),=C'RFHDR='                                            
         MVI   RFHCARD+6,C'0'                                                   
         MVC   RFHCARD+7(71),RFHCARD+6                                          
         LA    RF,RQHITRM                                                       
         GOTO1 HEXOUT,DMCB,(RF),CARD,36,=C'TOG'                                 
         MVC   RFHCARD+6(72),CARD                                               
*                                                                               
RFHDR3   MVC   CARD,RFHCARD        RELEASE CARD                                 
*&&UK*&& CLI   SPOOKSYS,C'A'                                                    
*&&UK*&& BE    RFHDR4                                                           
*&&UK*&& CLI   SPOOKSYS,C'M'                                                    
*&&UK*&& BE    RFHDR4                                                           
         CLI   SPOOKSYS,C'C'                                                    
         BE    RFHDR4                                                           
         B     RFHDRX                                                           
RFHDR4   BAS   RE,PUT                                                           
*                                                                               
RFHDRX   XIT1                                                                   
         DROP  R3,R4                                                            
         EJECT                                                                  
* ROUTINE TO SET SOFT VALUES                                                    
*                                                                               
SETSOFT  NTR1                                                                   
         XC    SOFTNTRY,SOFTNTRY                                                
         LA    R2,SOFTABLE                                                      
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
SETPRG   MVC   WORK(2),SPOOKEOD                                                 
         BR    RE                                                               
SETSYS   MVC   WORK(2),SPOOKSYS                                                 
         BR    RE                                                               
SETLG1   MVC   WORK(7),LOGOS                                                    
         BR    RE                                                               
SETLG2   MVC   WORK(7),LOGOS+7                                                  
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
*HANDLE JCL FROM CONTROL FILE. EXIT WITH CC=NEQ IF JCL BOOK NOT FOUND.          
*                                                                               
GETJCL   NTR1                                                                   
         CLI   SPOOKWEN,8          TEST MQIO EXTERNAL JOB REQUEST               
         BE    GJY                 IF SO EXIT WITH NO JCL                       
         CLI   MOREJCL,C'Y'                                                     
         BE    GJ2                                                              
         MVI   IF,C'Y'                                                          
         LA    R4,BOOKBUFF         SET UP BOOK KEY                              
         XCEF  BOOKBUFF,1112                                                    
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
         BNZ   *+6                 YES - ERASE CARDS & KEY                      
         DC    H'0'                                                             
         GOTO1 POWWOW,DMCB,=C'SCH',=C'END',0,0                                  
         XC    JESKEY,JESKEY                                                    
         B     GJN                                                              
*                                                                               
GJ2      GOTO1 GETBOOK,DMCB,BOOKBUFF,CARD,DATAMGR                               
         TM    8(R1),X'80'                                                      
         BO    GJ6                                                              
         CLI   8(R1),0                                                          
         BE    GJ3                                                              
         TM    8(R1),X'12'         TEST NOT FOUND                               
         BNZ   *+6                 YES - ERASE CARDS & KEY                      
         DC    H'0'                                                             
         GOTO1 POWWOW,DMCB,=C'SCH',=C'END',0,0                                  
         XC    JESKEY,JESKEY                                                    
         B     GJN                                                              
GJ3      CLC   CARD(2),=C'$$'      TEST DDS JCL STATEMENT                       
         BNE   *+10                                                             
         MVC   CARD(2),MVSJOB      YES - CONVERT TO MVS                         
         CLC   CARD(13),=C'REQUESTS HERE'                                       
         BE    GJ10                                                             
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
         BAS   RE,PUT                                                           
         B     GJ2                                                              
*                                                                               
GJ6      XC    BOOKBUFF(25),BOOKBUFF                                            
         B     GJY                                                              
         EJECT                                                                  
*EXTRA CONTROLLER CARDS                                                         
*                                                                               
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
GJ12     BAS   RE,PUT                                                           
*                                                                               
         MVC   CARD(5),=C'LOGO='                                                
         MVC   CARD+5(14),LOGOS                                                 
         MVC   CARD+19(8),JESJOB                                                
         MVC   CARD+33(7),=C'ORIGIN='                                           
         EDIT  (2,ID),(5,CARD+40),FILL=0                                        
         BAS   RE,PUT                                                           
*                                                                               
         CLI   SPOOKSYS,C'A'                                                    
         BNE   GJ14                                                             
         GOTO1 DATAMGR,DMCB,=C'DTFADD',=C'ACCOUNT'                              
         L     RE,12(R1)           GET A(ACCOUNT DCB)                           
         TM    ISFTYPE-ISDTF(RE),ISFTEMU                                        
         BZ    GJ16                ACCFILE IS NOT EMULATED                      
         MVC   CARD(10),=C'DMPARM=EMU'                                          
         BAS   RE,PUT                                                           
         B     GJ16                                                             
*                                                                               
GJ14     CLC   ORIGNADD,SPACES                                                  
         BE    GJ16                                                             
         MVC   CARD(7),=C'ORIGIN='                                              
         MVC   CARD+7(66),ORIGNADD                                              
         BAS   RE,PUT                                                           
*                                                                               
GJ16     MVC   CARD(10),=C'INPUT=CARD'                                          
         BAS   RE,PUT                                                           
*                                                                               
         MVC   CARD(7),=C'FACPAK='                                              
         MVC   CARD+7(1),FACID     CAME FROM SSB ORIGINALLY                     
         BAS   RE,PUT                                                           
*                                                                               
GJ17     L     RF,AUTL             TEST IF BILLING INITIATED                    
         TM    TSTAT1-UTLD(RF),TSTATBIL                                         
         BZ    GJ17X                                                            
         L     RF,ASYSFAC                                                       
         L     RF,VSSB-SYSFACD(RF)                                              
         L     RF,SSBTKADR-SSBD(RF)                                             
         LA    RF,TCBBILL-TCBD(RF) RF=A(BILLING REF IN TCB)                     
         CLC   0(L'TCBBILL,RF),=XL12'00'                                        
         BE    GJ17X                                                            
         MVC   CARD(8),=C'BILLREF='                                             
         GOTO1 HEXOUT,DMCB,(RF),CARD+8,12,=C'TOG'                               
         BAS   RE,PUT                                                           
GJ17X    EQU   *                                                                
*                                                                               
         MVC   CARD(16),=C'DIRECT=SPP,1S  ,'                                    
         MVC   CARD+7(3),SPOOKDID                                               
         MVI   CARD+21,C','                                                     
         MVI   CARD+27,C','                                                     
         MVI   CARD+29,C','                                                     
         MVI   CARD+31,C','                                                     
         MVI   CARD+33,C','                                                     
         MVI   CARD+55,C','                                                     
         MVI   CARD+67,C','                                                     
*                                                                               
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
         BZ    GJ19                                                             
*                                  CALL PQPROF, A(DATA) IN RETURN PARM          
         GOTO1 PQPROF,DMCB,PROFKEY,0,(RF)                                       
         CLI   8(R1),0                                                          
         BNE   GJ19                IO ERROR                                     
*                                                                               
         L     RF,4(R1)            PICK UP A(DATA) IN REMOTED FORMAT            
         LA    RF,0(RF)            AND FILL IN DIRECT= CARD                     
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
         MVC   SVRETAIN,REMOTRET   SAVE PQ RETENTION CLASS                      
         MVC   SVSQLCOD,REMOTSQL   SAVE PQ SQL TRANSFORM CODE                   
*                                                                               
         MVI   CARD+28,C' '        ALWAYS PUT BLANK INTO PQCLASS                
         MVI   CARD+32,C'1'        TEMP FIX (ALWAYS GENERATE ONE COPY)          
         DROP  RF                                                               
*                                                                               
GJ19     LA    RF,SPOOKUID                                                      
         NC    SPOOKDES,SPOOKDES                                                
         BZ    *+8                                                              
         LA    RF,SPOOKDES                                                      
         EDIT  (2,(RF)),(5,CARD+16),FILL=0                                      
GJ19A    CLC   SPOOKFRM(3),=C'FO=' SET FORM CODE IF DEFINED                     
         BNE   GJ19B                                                            
         MVC   CARD+11(4),SPOOKFRM+3                                            
         B     GJ19X                                                            
*                                                                               
GJ19B    CLC   SPOOKXT(3),=C'XT='  TEST IF EXTENDED SPOOK                       
         BNE   GJ19X                                                            
         MVC   CARD+22(2),=C'Q,'                                                
         CLC   SPOOKFNO,SPACES                                                  
         BNH   *+10                                                             
         MVC   CARD+11(4),SPOOKFNO FORM CODE                                    
         CLC   SPOOKSUB,SPACES                                                  
         BNH   *+10                                                             
         MVC   CARD+68(2),SPOOKSUB PQ FORM                                      
*                                                                               
GJ19C    TM    SPOOKTY,X'10'       TEST DOWN LOADABLE REPORT                    
         BNO   GJ19D                                                            
         MVI   CARD+24,C'D'                                                     
         B     GJ19E                                                            
*                                                                               
GJ19D    TM    SPOOKTY,X'08'       TEST IF TO BE CONVERTED TO SQL               
         BNO   GJ19E                                                            
         MVI   CARD+24,C'S'                                                     
         MVC   WORK(11),SPACES     C'SPP  @ABCDEF' IS SQL REPORT NAME           
         MVC   WORK(3),PROFKEY                                                  
         CLC   SPOOKSQL,SPACES     TEST IF CALLER PASSED TRANSFORM CODE         
         BNH   GJ19D1                                                           
         MVI   WORK+5,C'@'                                                      
         MVC   WORK+6(5),SPOOKSQL                                               
         B     GJ19D2                                                           
GJ19D1   CLC   SVSQLCOD,SPACES     TEST IF SQL CODE IN REPORT PROFILE           
         BNH   GJ19D2                                                           
         MVI   WORK+5,C'@'                                                      
         MVC   WORK+6(5),SVSQLCOD                                               
GJ19D2   MVC   CARD+56(11),WORK    SET DESCRIPTION TO CONTAIN SQL INFO          
*                                                                               
GJ19E    TM    SPOOKSTA,X'02'      TEST INVISIBLE REPORT                        
         BNO   *+8                                                              
         MVI   CARD+25,C'I'        INVISIBLE                                    
*                                                                               
GJ19F    CLI   SVRETAIN,0          TEST IF VALID RETAIN CLASS                   
         BE    GJ19X                                                            
         MVC   CARD+70(1),SVRETAIN                                              
*                                                                               
GJ19X    BAS   RE,PUT                                                           
         CLI   SVRETAIN,0                                                       
         BE    GJ20                                                             
         MVC   CARD(7),=C'RETAIN=' SET RETAIN=X IF REMOTRET SET                 
         MVC   CARD+7(1),SVRETAIN                                               
         BAS   RE,PUT                                                           
*                                                                               
GJ20     L     RF,AREQCRDS         A(REQUEST CARDS)                             
         CLI   0(RF),C'='          IF CARD BEGINS WITH EQUALS SIGN,             
         BNE   GJ25                THEN IT'S A CONTROL CARD                     
         MVC   CARD(79),1(RF)                                                   
         BAS   RE,PUT              PUT THE CONTROL CARD WITHOUT '='             
         ZIC   R0,REQCON                                                        
         BCTR  R0,0                                                             
         STC   R0,REQCON           DECREMENT NUMBER OF REQUEST CARDS            
         LA    RF,80(RF)                                                        
         ST    RF,AREQCRDS         BUMP TO NEXT CARD                            
         B     GJ20                LOOK FOR MORE CONTROL CARDS                  
*                                                                               
GJ25     MVC   CARD(2),=C'/*'                                                   
         BAS   RE,PUT                                                           
         B     GJY                                                              
*                                                                               
GJN      LTR   RB,RB               SET CC=NEQ ON ERROR                          
         B     XIT                                                              
GJY      CR    RB,RB               SET CC=EQ IF OK                              
         B     XIT                                                              
         EJECT                                                                  
*CHECK IF CONDITIONS                                                            
*                                                                               
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
         B     GJ2                                                              
*                                                                               
IFT2     CLC   CARD+10(2),SPOOKAGY                                              
         BNE   *+8                                                              
         MVI   IF,C'Y'                                                          
         B     GJ2                                                              
*                                                                               
IFT4     MVC   DUB(2),SPOOKUID                                                  
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
LASTJCL  NTR1                                                                   
         MVC   CARD,SPACES                                                      
         MVC   CARD(2),=C'**'                                                   
         BAS   RE,PUT                                                           
         B     XIT                                                              
         EJECT                                                                  
*ROUTINE TO OUTPUT THE CARDS                                                    
*                                                                               
PUT      NTR1                                                                   
         OC    ASPSV,ASPSV         TEST FIRST PUT                               
         BNZ   PUT3X               NO IRDR SAVE AREA ALREADY AVAIL              
         XC    JESKEY,JESKEY                                                    
         MVC   JESKUID,SPOOKDES    BUILD JES KEY                                
         OC    JESKUID,JESKUID                                                  
         BNZ   *+10                                                             
         MVC   JESKUID,SPOOKUID                                                 
         MVC   JESKDID,SPOOKDID                                                 
         MVI   JESKCLA,C'Q'                                                     
         CLI   SPOOKWEN,5          TEST UPDATIVE SOON                           
         BNE   *+8                                                              
         OI    JESKTYPE,X'80'                                                   
         CLI   SPOOKWEN,6          TEST LUNATIC SOON                            
         BNE   *+8                                                              
         OI    JESKTYPE,X'60'                                                   
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
PUT1A    TM    SPOOKTY,X'10'       TEST IF DOWNLOADABLE                         
         BZ    PUT1B                                                            
         OI    JESKTYPE,X'10'                                                   
         B     PUT1C                                                            
PUT1B    TM    SPOOKTY,X'08'       TEST IF TO BE CONVERTED TO SQL               
         BZ    *+8                                                              
         OI    JESKTYPE,X'08'                                                   
PUT1C    CLC   SPOOKSUB,SPACES     PQ FORM PASSED IN SUB FIELD                  
         BNH   *+10                                                             
         MVC   JESKSUB,SPOOKSUB                                                 
*                                                                               
PUT2     GOTO1 POWWOW,DMCB,=C'SCH',=C'ADR',JESKEY,ASPSV                         
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
PUT3X    BAS   RE,PUTSCN           SCAN CARD AND EXTRACT CARD TYPE              
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
         MH    RE,=H'3'                                                         
         LA    RE,DQPQTAB(RE)                                                   
         MVC   DQDDN(3),0(RE)      EXTRACT QDQ DD AND PRTQ DD NUMS              
         OC    DQDDN(3),DQDDN      TEST IF FAC SYS NEEDS TST DD CARDS           
         BZ    PUTW                NO                                           
         CLI   PRTQ,C'Y'           BUT DO WE WANT THEM                          
         BNE   PUTW                NO                                           
         MVC   JESCARD,CARD        YES OUTPUT THE EXEC CARD HERE                
         GOTO1 POWWOW,DMCB,=C'SCH',=C'JES',JESKEY,JESCARD                       
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
         GOTO1 POWWOW,DMCB,=C'SCH',=C'JES',JESKEY,JESCARD                       
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
         MH    RE,=H'80'                                                        
         LA    RE,QDCDD1(RE)                                                    
         MVC   CARD,0(RE)                                                       
         MVC   JESCARD,CARD                                                     
         GOTO1 POWWOW,DMCB,=C'SCH',=C'JES',JESKEY,JESCARD                       
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
         MH    RE,=H'80'                                                        
         LA    RE,PRTQDD1(RE)                                                   
         MVC   CARD,0(RE)                                                       
         LA    R5,1                R5=PRTQ NUMBER                               
PUT5B4   LR    RF,R5                                                            
         BCTR  RF,0                                                             
         LA    RF,IDSPQS(RF)       RF=A(PRTQ LETTER)                            
         MVC   CARD+06(1),0(RF)                                                 
         MVC   CARD+28(1),0(RF)                                                 
         MVC   JESCARD,CARD                                                     
         GOTO1 POWWOW,DMCB,=C'SCH',=C'JES',JESKEY,JESCARD                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,SPSVTCNT         BUMP TOTAL CARD COUNT                        
         LA    RE,1(RE)                                                         
         STC   RE,SPSVTCNT                                                      
         LA    R5,1(R5)            BUMP PRTQ FILE NUMBER                        
         CH    R5,NUMPQS                                                        
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
         GOTO1 POWWOW,DMCB,=C'SCH',=C'JES',JESKEY,JESCARD                       
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
* FIND A SPARE ENTRY IN THE JOB TABLE                                           
* EXIT - TCBSJENT CONTAINS A(ENTRY), NULLS IF TABLE IS FULL                     
*                                                                               
FINDNTRY NTR1                                                                   
*                                                                               
         GOTO1 PROTOFF             TURN OFF FACPAK PROTECTION                   
*                                                                               
         L     RF,ASYSFAC                                                       
         L     RF,VSSB-SYSFACD(RF)                                              
         L     R3,SSBTKADR-SSBD(RF)                                             
         XC    TCBSJENT-TCBD(,R3),TCBSJENT-TCBD(R3)                             
         L     R1,SSBAJOB-SSBD(RF) A(JOB TABLE)                                 
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
*                                                                               
NEXTNTRY L     R0,0(R1)            HIGH-ORDER BIT OF R0 SHOWS STATUS            
         LTR   R2,R0               IS ENTRY ALREADY TAKEN?                      
         BM    *+16                YES                                          
         O     R2,=X'80000000'     R2 = THE SAME ENTRY, MARKED USED             
         CS    R0,R2,0(R1)         IS ENTRY STILL FREE?                         
         BE    *+12                YES, BUT NOW IT'S OURS                       
         BXLE  R1,RE,NEXTNTRY      TRY NEXT ENTRY                               
         B     *+14                TABLE IS FULL                                
         ST    R1,TCBSJENT-TCBD(R3) A(THIS JOB TABLE ENTRY)                     
         XC    JOBPQKEY-JOBTABD(,R1),JOBPQKEY-JOBTABD(R1) CLEAR GARBAGE         
*                                                                               
         GOTO1 PROTON              TURN ON FACPAK PROTECTION                    
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* FILL IN THE REST OF THE JOB TABLE ENTRY WITH PERTINENT INFORMATION            
*                                                                               
FILLNTRY NTR1                                                                   
*                                                                               
         GOTO1 PROTOFF             TURN OFF FACPAK PROTECTION                   
*                                                                               
         L     R4,ASYSFAC          A(SYSFACS)                                   
         L     R4,VSSB-SYSFACD(R4) A(SSB)                                       
         USING SSBD,R4                                                          
         L     RF,SSBTKADR         A(TCB ENTRY)                                 
         USING TCBD,RF                                                          
         L     R3,TCBSJENT         A(THIS JOB TABLE ENTRY)                      
         USING JOBTABD,R3                                                       
         MVC   JOBPQKEY,TCBSJUSR                                                
         MVC   JOBPQCIA,TCBSJCIA                                                
*&&US*&& MVC   JOBCLASS,MAINCLAS   SAVE SUBMIT CLASS                            
*&&UK                                                                           
         MVC   JOBCLASS(1),JOBCLA  SAVE SUBMIT CLASS                            
         MVI   JOBCLASS+1,C' '                                                  
*&&                                                                             
         MVC   JOBPRTY,JOBPRI      PRIORITY                                     
         L     R2,TCBUTL           A(UTL ENTRY)                                 
         USING UTLD,R2                                                          
         MVC   JOBTERM,TNUM        TERMINAL NUMBER                              
         DROP  RF                                                               
*                                                                               
         SR    RE,RE                                                            
         CLC   TUSER,=AL2(32000)   PUBLIC REPORTS GO TO FIRST PRTQUE            
         BL    *+14                                                             
         CLC   TUSER,=AL2(32100)                                                
         BNH   FILLN10                                                          
         SR    RF,RF                                                            
         ICM   RF,3,JOBPQUSR       BUG FIX - WAS TUSER                          
         LH    R0,NUMPQS                                                        
         DR    RE,R0                                                            
FILLN10  LA    RE,IDSPQS(RE)                                                    
         MVC   JOBPQID,0(RE)       EBCDIC PRTQ ID                               
*                                                                               
         OI    SSBSTAT1,SSBSCHK1   MUST CHECKPOINT AREA 1                       
*                                                                               
         DROP  R2,R3,R4                                                         
*                                                                               
         GOTO1 PROTON              TURN ON FACPAK PROTECTION                    
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*ROUTINE TO READ CONTROL FILE                                                   
*                                                                               
READ     NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,IO,0                      
         CLI   DMCB+8,0                                                         
         BE    XIT                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
*ROUTINE TO GET ELEMENT IN CONTROL FILE RECORD                                  
*                                                                               
         GETEL (R6),28,ELCODE                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
FILLET   DC    C' 123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'                          
         EJECT                                                                  
*PRINT QUEUE FILE DATA AND MODEL JCL STATEMENTS                                 
*                                                                               
IDSPQS   DC    C'123456789ABCDEFG'                                              
*&&UK                                                                           
NUMPQS   DC    H'08'               UK HAS EIGHT PRTQ FILES                      
DQPQTAB  DC    AL1(0,0,0)          0                                            
         DC    AL1(1,0,1)          1 TST                                        
         DC    AL1(0,0,0)          2 AD1                                        
         DC    AL1(1,0,1)          3 TTS                                        
         DC    AL1(0,0,0)          4 AD2                                        
         DC    AL1(1,0,1)          5 NEW                                        
         DC    AL1(0,0,0)          6 AD3                                        
         DC    AL1(0,0,0)          7 ???                                        
         DC    AL1(0,0,0)          8 AD4                                        
         DC    AL1(1,0,1)          9 Y2K                                        
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
         DC    AL1(1,1,1)          9 FAUK                                       
         DC    AL1(0,0,0)         10 AD6                                        
         DC    AL1(1,1,1)         11 FRED                                       
         DC    AL1(0,0,0)         12 AD7                                        
         DC    AL1(0,0,0)         13 DARE                                       
         DC    AL1(0,0,0)         14 REPB                                       
*&&                                                                             
PRTQDD1  DC    CL80'//PRTQ1     DD  DSN=FAC.PRTQ10,DISP=SHR'                    
*                                                                               
QDQDD1   DC    CL80'//DDSQDQ    DD  DSN=FAC.DDSQDQ0,DISP=SHR'                   
*                                                                               
QDCDD1   DC    CL80'//DDSQDC    DD  DSN=FAC.DDSQDC0,DISP=SHR'                   
         SPACE 1                                                                
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
         EJECT                                                                  
*SUBSIDIARY SCAN TABLE FOR SOFT JCL STATEMENTS                                  
*                                                                               
SOFTABLE DS    0XL20                                                            
         DC    AL1(7,2),AL3(SETAGY),CL15'&&AGENCY'                              
         DC    AL1(8,1),AL3(SETCMP),CL15'&&COMPANY'                             
         DC    AL1(7,1),AL3(SETLDG),CL15'&&LEDGER'                              
         DC    AL1(4,2),AL3(SETREP),CL15'&&REP'                                 
         DC    AL1(4,8),AL3(SETDAT),CL15'&&IPL'                                 
         DC    AL1(8,2),AL3(SETPRG),CL15'&&PROGRAM'                             
         DC    AL1(7,2),AL3(SETSYS),CL15'&&SYSTEM'                              
         DC    AL1(6,7),AL3(SETLG1),CL15'&&LOGO1'                               
         DC    AL1(6,7),AL3(SETLG2),CL15'&&LOGO2'                               
         DC    AL1(7,6),AL3(SETORG),CL15'&&ORIGIN'                              
         DC    AL1(4,2),AL3(SETDAY),CL15'&&DAY'                                 
         DC    AL1(8,2),AL3(SETHEX),CL15'&&HEXCOMP'                             
         DC    AL1(0)                                                           
         SPACE 1                                                                
EQUJOB   EQU   X'81'               EQUATES FOR JCLTYPE CARD TYPES               
EQUEXEC  EQU   X'82'                                                            
EQUDD    EQU   X'84'                                                            
EQUEOJ   EQU   X'88'                                                            
EQUCOMM  EQU   X'90'                                                            
EQUJFORM EQU   X'41'                                                            
EQUJMAIN EQU   X'42'                                                            
EQUJNET  EQU   X'44'                                                            
         EJECT                                                                  
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
         SPACE 1                                                                
* DDSPOOK                                                                       
       ++INCLUDE DDSPOOK                                                        
         EJECT                                                                  
SPOOND   DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
RELO     DS    A                                                                
DATAMGR  DS    V                                                                
GETBOOK  DS    V                                                                
HEXOUT   DS    V                                                                
REPLACE  DS    V                                                                
POWWOW   DS    V                                                                
SWITCH   DS    V                                                                
PQPROF   DS    V                                                                
PROTON   DS    V                                                                
PROTOFF  DS    V                                                                
ASYSFAC  DS    A                                                                
AUTL     DS    A                                                                
WORK     DS    XL32                                                             
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
FACID    DS    CL1                 FACPAK ONE CHARACTER ID                      
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
         DS    CL15                SPARE                                        
*                                                                               
JESCARD  DS    CL80                                                             
*                                                                               
TODAY    DS    CL8                 DATE C'DD/MM/YY' OR C'MM/DD/YY'              
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
SPACES   DS    CL132                                                            
ELCODE   DS    CL1                                                              
IF       DS    CL1                                                              
MOREJCL  DS    CL1                                                              
EXEC     DS    CL1                                                              
MAINCLAS DS    CL2                                                              
FILSYS   DS    XL1                 SYSTEM NUMBER (1-15)                         
FILSEN   DS    XL1                 SYSTEM SE NUMBER (1-255)                     
FILSET   DS    XL1                 SYSTEM FILESET NUMBER (1-36)                 
SYSID    DS    CL1                 FACFAC SYSTEM ID NUMBER                      
ORIGNADD DS    CL66                                                             
LOGOS    DS    CL14                                                             
SVRETAIN DS    C                   PQ RETENTION CLASS FROM PQPROF               
SVSQLCOD DS    CL5                 SQL TRANSFORM CODE FROM PQPROF               
*                                                                               
AREQCRDS DS    A                   A(REQUEST CARDS FROM PARAMETER LIST)         
SOFTNTRY DS    F                                                                
SOFTWORK DS    20XL32                                                           
IO       DS    1000C                                                            
BOOKBUFF DS    1112C                                                            
SPOONDL  EQU   *-SPOOND                                                         
         SPACE 2                                                                
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
         SPACE 1                                                                
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DMREQHDRA                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMREQHDRA                                                      
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004FASPOONS  05/01/02'                                      
         END                                                                    
