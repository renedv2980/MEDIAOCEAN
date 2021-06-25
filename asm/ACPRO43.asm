*          DATA SET ACPRO43    AT LEVEL 060 AS OF 09/12/02                      
*PHASE T60B43A,*                                                                
         TITLE 'T60B43 - JOB ESTIMATE - EDIT WORKCODE ESTIMATE COLUMNS'         
T60B43   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**EDWC**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         SPACE 1                                                                
         ST    R2,RELO                                                          
         BAS   RE,EDT                                                           
         XMOD1 1                                                                
         EJECT                                                                  
******************************************************************              
* SUB-ROUTINE TO EDIT THE SCREEN, UPDATE ESTIMATE RECORDS, AND   *              
* HANDLE WORKCODE RELATED SCREEN SWAPS                           *              
******************************************************************              
         SPACE 1                                                                
EDT      NTR1  ,                                                                
         L     R2,AFSTSEL          R2=A(SELECT FIELD HEADER)                    
         ZIC   R3,SVNWCS           R3=LOOP COUNTER                              
*                                                                               
* PREVIEW THE SELECT FIELDS FOR CORRECT SYNTAX                                  
*                                                                               
EDT2     BAS   RE,SETLIN                                                        
         L     R2,ASEL                                                          
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    EDT4                NO                                           
         CLI   8(R2),C'*'          TEST EDITED ALREADY                          
         BE    EDT4                                                             
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),1                                                          
         BNE   EDT3                                                             
         CLI   WORK,C'T'           TEST FOR TEXT MAINT SWAP                     
         BE    EDT4                                                             
         CLI   WORK,C'Z'           TEST FOR ZOOM TO DETAIL                      
         BE    EDT4                                                             
         CLI   WORK,C'O'           TEST FOR OPT MAINT WC SWAP                   
         BE    EDT4                                                             
         B     ERREND                                                           
*                                                                               
EDT3     CLI   WORK,C'T'           TEST FOR 'T'=TEXT MAINT                      
         BNE   ERREND                                                           
*                                                                               
         LA    RE,WHERETAB                                                      
         LA    R0,WHERES           ONLY THE WHERE FIELD CAN FOLLOW              
         CLC   WORK+1(2),0(RE)                                                  
         BE    EDT4                                                             
         LA    RE,L'WHERETAB(RE)                                                
         BCT   R0,*-14                                                          
         B     ERREND                                                           
*                                                                               
EDT4     L     R2,ANEXTSEL                                                      
         BCT   R3,EDT2                                                          
*                                                                               
EDT5     BAS   RE,EDTSCR                                                        
*                                                                               
         L     R2,AFSTSEL          R2=A(SELECT FIELD HEADER)                    
         ZIC   R3,SVNWCS           R3=LOOP COUNTER                              
         LA    R5,SVTABLE          R5=A(SAVE TABLE ENTRY)                       
*                                                                               
EDT6     CLI   5(R2),0                                                          
         BE    EDT10                                                            
         CLI   8(R2),C'*'          TEST ALREADY DEALT WITH                      
         BE    EDT10                                                            
*                                                                               
         GOTO1 ANY                                                              
         MVC   8(3,R2),SPACES                                                   
         MVI   8(R2),C'*'                                                       
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         MVI   PFKEY,0                                                          
         CLI   WORK,C'T'                                                        
         BE    EDT8                                                             
         CLI   WORK,C'Z'                                                        
         BE    EDT7                                                             
*                                                                               
         GOTO1 VCALL,WORK,RECNOPT,ACTNMNT,=C',',=C',',(6,CLICODE),     X        
               (6,PRODCODE),(6,JOBNUM),=C',',=C',',=C',',(2,0(R5)),0            
*                                                                               
EDT7     GOTO1 VCALL,WORK,RECNJOB,ACTNDET,(6,CLICODE),(6,PRODCODE),    X        
               (6,JOBNUM),(3,0(R5)),(1,DFTOPT),0                                
*                                                                               
EDT8     MVC   WHERE,WORK+1        EXTRACT WHERE VALUE                          
         GOTO1 VCALL,WORK,RECNTEXT,ACTNMNT,=C',',=C',',(L'CLICODE,     X        
               CLICODE),(L'PRODCODE,PRODCODE),(L'JOBNUM,JOBNUM),=C',', X        
               =C',',=C',',(3,0(R5)),=C'E',(L'WHERE,WHERE),0                    
*                                                                               
EDT10    BAS   RE,SETLIN                                                        
         L     R2,ANEXTSEL                                                      
         LA    R5,L'SVTABLE(R5)                                                 
         BCT   R3,EDT6                                                          
*                                                                               
EDTX     B     XIT                                                              
         SPACE 2                                                                
* TABLE OF WHERE VALUES FOR TEXT MAINT SWAP                                     
*                                                                               
WHERETAB DS    0CL2                                                             
         DC    C'H '                                                            
         DC    C'F '                                                            
WHERES   EQU   (*-WHERETAB)/L'WHERETAB                                          
         EJECT                                                                  
********************************************************************            
* SUB-ROUTINE TO EDIT THE WORKCODE COLUMNS--CALLED FROM EDT--      *            
* AT ENTRY, PASS=1 OR 2                                            *            
********************************************************************            
         SPACE 1                                                                
         USING COLD,R4                                                          
EDTSCR   NTR1  ,                                                                
         MVI   PASS,1              FIRST PASS FOR SYNTAX EDIT                   
         L     R2,AFSTNAME                                                      
         ST    R2,ATHISNAM         SET NAME FIELD POINTER                       
         MVC   ATHISNM2,ASECNAME   AND SECOND NAME FIELD                        
         L     R2,AFSTSEL                                                       
         BAS   RE,SETLIN                                                        
         L     R2,ADATA1                                                        
         ST    R2,ATHISCOL                                                      
         LA    R4,COLINDS          R4=A(COLUMN INDICATORS)                      
         ZIC   R6,NCOLS            R6=LOOP COUNTER                              
*                                                                               
EDTSCR2  CLI   COLATTB,COLAUNP     TEST FOR UNPROTECTED COLUMN                  
         BNE   *+8                                                              
         BAS   RE,EDTCOL                                                        
*                                                                               
         L     R2,ATHISNAM                                                      
         BAS   RE,BUMP                                                          
         ST    R2,ATHISNAM                                                      
         L     R2,ATHISNM2                                                      
         BAS   RE,BUMP                                                          
         ST    R2,ATHISNM2                                                      
         L     R2,ATHISCOL                                                      
         BAS   RE,BUMP                                                          
         ST    R2,ATHISCOL                                                      
         LA    R4,COLLENQ(R4)                                                   
         BCT   R6,EDTSCR2                                                       
*                                                                               
EDTSCR4  L     R2,AFSTNAME                                                      
         ST    R2,ATHISNAM         SET NAME FIELD POINTER                       
         MVC   ATHISNM2,ASECNAME                                                
         L     R2,AFSTSEL                                                       
         BAS   RE,SETLIN                                                        
         L     R2,ADATA1                                                        
         ST    R2,ATHISCOL                                                      
         LA    R4,COLINDS          R4=A(COLUMN INDICATORS)                      
         ZIC   R6,NCOLS            R6=LOOP COUNTER                              
*                                                                               
EDTSCR6  CLI   COLATTB,COLAUNP                                                  
         BNE   EDTSCR8                                                          
*                                                                               
         MVI   PASS,1                                                           
         BAS   RE,EDTCOL                                                        
         MVI   PASS,2                                                           
         BAS   RE,EDTCOL                                                        
*                                                                               
         CLI   UPDATE,C'Y'         TEST TO UPDATE RECORD                        
         BNE   EDTSCR8                                                          
*                                                                               
         CLI   UPACT,C'Y'          TEST TO UPDATE ACTIVITY ELEMENT              
         BNE   EDTSCR7                                                          
*                                                                               
         MVI   ELCODE,EUPELQ       ESTIMATE UPDATE ELEMENT                      
         GOTO1 REMELEM                                                          
*                                                                               
         GOTO1 GETFACT,DMCB,(1,0)                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
*                                                                               
         LA    RE,ELEM                                                          
         USING EUPELD,RE                                                        
         MVI   EUPEL,EUPELQ                                                     
         MVI   EUPLN,EUPLNQ                                                     
         MVC   EUPLAST,TODAYP                                                   
         MVC   EUPERS,TWAALIAS                                                  
         MVC   EUPTIME,FATIME+1                                                 
         GOTO1 ADDELEM                                                          
         GOTO1 WRITE                                                            
*                                                                               
         BAS   RE,SPECWC           HANDLE SPECIAL WORKCODES                     
         B     EDTSCR8                                                          
*                                                                               
EDTSCR7  GOTO1 WRITE                                                            
*                                                                               
EDTSCR8  L     R2,ATHISNAM                                                      
         BAS   RE,BUMP                                                          
         ST    R2,ATHISNAM                                                      
         L     R2,ATHISNM2                                                      
         BAS   RE,BUMP                                                          
         ST    R2,ATHISNM2                                                      
         L     R2,ATHISCOL                                                      
         BAS   RE,BUMP                                                          
         ST    R2,ATHISCOL                                                      
         LA    R4,COLLENQ(R4)                                                   
         BCT   R6,EDTSCR6                                                       
*                                                                               
EDTSCRX  B     XIT                                                              
         DROP  R1,RE                                                            
         EJECT                                                                  
*******************************************************************             
* SUB-ROUTINE TO EDIT A COLUMN ON THE SCREEN--CALLED FROM EDTSCR  *             
* AT ENTRY, R4=A(COLINDS ENTRY), ATHISNAM=A(EST NAME FIELD),      *             
*           ATHISCOL=A(FIRST DATA FIELD FOR COLUMN)               *             
*******************************************************************             
         SPACE 1                                                                
EDTCOL   NTR1  ,                                                                
         MVI   UPDATE,C'N'         INITIALIZE UPDATE RECORD SWITCH              
         MVI   UPACT,C'N'          AND UPDATE ACTIVITY ELEMENT SWITCH           
         CLI   PASS,2              TEST FOR SECOND PASS                         
         BE    EDTCOL2             YES                                          
         GOTO1 AGETEST,DMCB,(RC),COLEST                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EDTCOL2  BAS   RE,EDTNAME                                                       
         ZIC   R3,SVNWCS           R3=WORKCODE COUNTER                          
         LA    R5,SVTABLE                                                       
*                                                                               
EDTCOL4  TM    4(R2),X'20'         TEST FOR CHANGED FIELD                       
         BO    EDTCOL6             NO                                           
         MVI   UPDATE,C'Y'                                                      
         MVI   UPACT,C'Y'                                                       
         BAS   RE,EDTWC                                                         
*                                                                               
EDTCOL6  LA    R1,NDATAFLD         BUMP AHEAD TO FIELD UNDERNEATH               
         BAS   RE,BUMP                                                          
         BCT   R1,*-4                                                           
         LA    R5,L'SVTABLE(R5)                                                 
         BCT   R3,EDTCOL4                                                       
*                                                                               
EDTCOLX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
********************************************************************            
* SUB-ROUTINE TO EDIT THE NAMES FIELDS--CALLED FROM EDTCOL--       *            
********************************************************************            
         SPACE 1                                                                
EDTNAME  NTR1  ,                                                                
         L     R2,ATHISNAM                                                      
         TM    4(R2),X'20'         TEST IF FIELD CHANGED                        
         BZ    EDTNAME2            YES                                          
         L     R2,ATHISNM2         TEST IF SECOND FIELD CHANGED                 
         TM    4(R2),X'20'                                                      
         BO    EDTNAMEX            NO                                           
*                                                                               
EDTNAME2 L     R2,ATHISNAM                                                      
         MVI   BYTE,1                                                           
         BAS   RE,NAME             MAINTAIN NAME ELEMENT                        
*                                                                               
         L     R2,ATHISNM2                                                      
         MVI   BYTE,2                                                           
         BAS   RE,NAME             MAINTAIN NAME ELEMENT                        
*                                                                               
EDTNAMEX B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO MAINTAIN THE NAME ELEMENT                                      
*                                                                               
NAME     ST    RE,SAVERE                                                        
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),('ACENELQ',AIO),(1,BYTE)                
         CLI   5(R2),0             TEST FOR INPUT IN FIELD                      
         BE    NAMEX               NO                                           
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ACEND,R6                                                         
         MVI   ACENEL,ACENELQ                                                   
         MVC   ACENNUM,BYTE                                                     
         GOTO1 ANY                                                              
         MVC   ACENAME(L'WORK),WORK                                             
         ZIC   R1,5(R2)                                                         
         LA    R1,ACENAME-ACEND+1(R1) RESTORE EL LENGTH                         
         STC   R1,ACENLEN                                                       
         GOTO1 ADDELEM                                                          
*                                                                               
NAMEX    MVI   UPDATE,C'Y'                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
********************************************************************            
* SUB-ROUTINE TO EDIT A WORKCODE FIELD--CALLED FROM EDTCOL--       *            
* AT ENTRY, R2=A(FIELD HEADER), R5=A(SVTABLE ENTRY)                *            
********************************************************************            
         SPACE 1                                                                
EDTWC    NTR1  ,                                                                
         MVI   ERROR,CLOSERR                                                    
         TM    JOBSTAT,X'40'       TEST FOR CLOSED JOB                          
         BO    ERREND              YES-NO CHANGES TO WORKCODES                  
*                                                                               
         ZAP   PREVCOMM,=P'0'                                                   
         ZAP   PREVNCOM,=P'0'      SET PREVIOUS VALUE TO ZERO                   
*                                                                               
         CLI   PASS,2              TEST FOR SECOND PASS                         
         BE    EDTWCA              YES-DO NOT DELETE ELEMENTS                   
*                                                                               
         ZAP   PREVHRS,=P'0'                                                    
         ZAP   PREVHRB,=P'0'                                                    
         ZAP   PREVHRN,=P'0'                                                    
         ZAP   PREVHRR,=P'0'                                                    
*                                                                               
EDTWCA   MVI   ELCODE,EDAELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   EDTWC2                                                           
*                                                                               
         USING EDAELD,R6                                                        
EDTWC0   TM    EDATYPE,EDATWORK                                                 
         BZ    *+14                                                             
         CLC   EDAWORK,0(R5)       IS THIS THE WORKCODE WE WANT?                
         BE    EDTWC00                                                          
         BAS   RE,NEXTEL                                                        
         BNE   EDTWC2                                                           
         B     EDTWC0                                                           
*                                                                               
EDTWC00  MVC   FULL(3),2(R6)       SAVE TYPE AND WORKCODE                       
         CLI   PASS,2              TEST FOR SECOND PASS                         
         BE    EDTWC2              YES-DO NOT DELETE ELEMENTS                   
*                                                                               
         CLI   EDALN,EDALNQ1                                                    
         BE    EDTWC1                                                           
         ZAP   PREVNCOM,EDANCOM                                                 
*                                                                               
         CLI   EDALN,EDALN4Q                                                    
         BL    EDTWC1                                                           
         ZAP   PREVHRS,EDAHOURS                                                 
*                                                                               
         CLI   EDALN,EDALN6Q                                                    
         BL    EDTWC1                                                           
         ZAP   PREVHRB,EDABHRS                                                  
         ZAP   PREVHRN,EDANHRS                                                  
         ZAP   PREVHRR,EDARHRS                                                  
*                                                                               
EDTWC1   ZAP   PREVCOMM,EDACOMM                                                 
         CLI   EDALN,EDALNQ1                                                    
         BE    *+10                                                             
         ZAP   PREVNCOM,EDANCOM                                                 
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),('EDAELQ',AIO),(3,FULL)                 
*                                                                               
EDTWC2   MVI   EDTSW,0                                                          
         GOTO1 PARSNIP,DMCB,(R2),BLOCK,('PSNNONLQ',EDTSEP)                      
         MVI   ERROR,INVALID                                                    
         CLI   8(R1),0             TEST FOR PARSNIP ERROR                       
         BE    EDTWC3              NO                                           
         SR    R3,R3                                                            
         ICM   R3,7,9(R1)                                                       
         B     EDTWCR2                                                          
*                                                                               
EDTWC3   LA    R4,BLOCK                                                         
         USING PSND,R4                                                          
         ZAP   EDTCOMM,=P'0'                                                    
         ZAP   EDTNCOM,=P'0'                                                    
*                                                                               
EDTWC4   CLI   PSNTAG,PSNFLDQ                                                   
         BNE   EDTWCR                                                           
*                                                                               
         L     R3,PSNCOMP                                                       
         ZIC   R6,PSNLEN                                                        
         CLI   0(R3),C'+'          TEST PLUS SIGN STARTS FIELD                  
         BE    EDTWC5              YES                                          
         CLI   0(R3),C'A'          TEST 'A'=ADD                                 
         BE    EDTWC5                                                           
         CLI   0(R3),C'S'          TEST 'S'=SUBTRACT                            
         BNE   EDTWC6                                                           
*                                                                               
EDTWC5   MVC   EDTSW,0(R3)         SAVE SWITCH VALUE                            
         LA    R3,1(R3)            YES-ADJUST PARSNIP BLOCK                     
         SH    R6,=H'1'                                                         
         BZ    EDTWCR                                                           
         STC   R6,PSNLEN                                                        
         ST    R3,PSNCOMP                                                       
*                                                                               
EDTWC6   MVI   COMMSW,C'Y'                                                      
         MVI   REFSW,C'N'          WORKCODE REFERENCE=NO                        
         BAS   RE,EDTVAL                                                        
         ZAP   EDTCOMM,DUB                                                      
         ICM   R4,15,PSNFLD                                                     
         BZ    EDTWC8              NO SECOND FIELD PRESENT                      
*                                                                               
         L     R3,PSNCOMP                                                       
         CLI   2(R5),0             TEST FOR SUFFIX WORKCODE                     
         BNE   EDTWCR              YES-ONLY ALLOW ONE FIELD                     
         CLI   REFSW,C'Y'          TEST WORKCODE REFERENCE IN FIRST             
         BE    EDTWCR              YES-NO MORE INPUT                            
         MVI   COMMSW,C'N'                                                      
         BAS   RE,EDTVAL                                                        
         ZAP   EDTNCOM,DUB                                                      
*                                                                               
EDTWC8   CLI   EDTSW,0             TEST UPDATING PREVIOUS                       
         BE    EDTWC10             NO                                           
*                                                                               
         CLI   PASS,2              BUT ONLY DO IT THE FIRST TIME                
         BE    EDTWC10                                                          
         CLI   EDTSW,C'S'          TEST FOR SUBTRACTION                         
         BE    EDTWC9              YES                                          
*                                                                               
         AP    EDTCOMM,PREVCOMM                                                 
         AP    EDTNCOM,PREVNCOM                                                 
         B     EDTWC10                                                          
*                                                                               
EDTWC9   ZAP   DUB,PREVCOMM        GET PREVIOUS COMMISSIONABLE                  
         SP    DUB,EDTCOMM         SUBTRACT INPUT                               
         ZAP   EDTCOMM,DUB                                                      
         ZAP   DUB,PREVNCOM                                                     
         SP    DUB,EDTNCOM                                                      
         ZAP   EDTNCOM,DUB                                                      
*                                                                               
EDTWC10  CLI   REFSW,C'Y'          TEST WORKCODE REFERENCE                      
         BE    EDTWC11             YES                                          
*                                                                               
         CLI   PASS,1              TEST SECOND PASS                             
         BE    EDTWC12             YES-ALL DONE                                 
         B     EDTWCX                                                           
*                                                                               
EDTWC11  CLI   PASS,2              WORKCODE REFERENCE                           
         BNE   EDTWCX              EXIT ON FIRST PASS                           
*                                                                               
EDTWC12  XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING EDAELD,R6                                                        
         MVI   EDAEL,EDAELQ                                                     
         MVI   EDALN,EDALNQ1                                                    
         MVI   EDATYPE,EDATWORK                                                 
         MVC   EDAWORK,0(R5)                                                    
         CLI   2(R5),0             TEST FOR SUFFIX WORKCODE                     
         BE    EDTWC14             NO                                           
*                                                                               
         MVI   EDALN,EDALNQ2        LONGER LENGTH                               
         ZAP   EDACOMM,PREVCOMM     INITIALIZE WITH PRIOR VALUES                
         ZAP   EDANCOM,PREVNCOM                                                 
         LA    RE,EDACOMM                                                       
         CLI   2(R5),C'C'          TEST FOR COMMISSIONABLE ONLY                 
         BE    *+8                 YES                                          
         LA    RE,EDANCOM                                                       
         ZAP   0(L'EDACOMM,RE),EDTCOMM                                          
         B     EDTWC20                                                          
*                                                                               
EDTWC14  ZAP   EDACOMM,EDTCOMM                                                  
         ZAP   EDANCOM,=P'0'                                                    
         CP    EDTNCOM,=P'0'                                                    
         BE    EDTWC15                                                          
         MVI   EDALN,EDALNQ2                                                    
         ZAP   EDANCOM,EDTNCOM                                                  
*                                                                               
EDTWC15  CP    PREVHRS,=P'0'       ANY HOURS                                    
         BE    EDTWC16             NO                                           
         ZAP   EDAHOURS,PREVHRS                                                 
         MVI   EDALN,EDALN4Q                                                    
         OI    EDATYPE,EDATTEST                                                 
*                                                                               
EDTWC16  CP    PREVHRB,=P'0'                                                    
         BE    EDTWC17                                                          
         ZAP   EDABHRS,PREVHRB                                                  
         OI    EDATYPE,EDATBNR                                                  
         MVI   EDALN,EDALN6Q                                                    
*                                                                               
EDTWC17  CP    PREVHRN,=P'0'                                                    
         BE    EDTWC18                                                          
         ZAP   EDANHRS,PREVHRN                                                  
         OI    EDATYPE,EDATBNR                                                  
         MVI   EDALN,EDALN6Q                                                    
*                                                                               
EDTWC18  CP    PREVHRR,=P'0'                                                    
         BE    EDTWC20                                                          
         ZAP   EDARHRS,PREVHRR                                                  
         OI    EDATYPE,EDATBNR                                                  
         MVI   EDALN,EDALN6Q                                                    
*                                                                               
EDTWC20  LA    R0,1                                                             
         BNE   *+8                                                              
         LA    R0,2                                                             
*                                                                               
EDTWC21  LA    RE,EDACOMM                                                       
         CP    0(L'EDACOMM,RE),=P'0'                                            
         BNE   EDTWC22                                                          
         LA    RE,L'EDACOMM(RE)                                                 
         BCT   R0,*-14                                                          
         B     EDTWCX              NO NEED TO ADD AN ELEMENT                    
*                                                                               
EDTWC22  GOTO1 ADDELEM                                                          
*                                                                               
EDTWCX   B     XIT                                                              
*                                                                               
EDTWCR   LA    RE,8(R2)            RE=A(START OF FIELD)                         
         SR    R3,RE                                                            
*                                                                               
EDTWCR2  STC   R3,ERRNDX                                                        
         B     ERREND                                                           
         SPACE 2                                                                
EDTSEP   DC    AL1(1),C'*'                                                      
         DC    AL1(1),C','                                                      
         DC    AL1(1),C' '                                                      
         DC    AL1(1),C' '                                                      
         EJECT                                                                  
*****************************************************************               
* SUB-ROUTINE TO EDIT AN AMOUNT FIELD --CALLED FROM EDTWC--     *               
* AT ENTRY, R4=A(PARSNIP BLOCK), R5=A(SVTABLE ENTRY)            *               
*           PASS=1 OR 2, COMMSW=Y/N FOR COMMISSIONABLE EDIT     *               
* ON EXIT, DUB CONTAINS EDITED VALUE                            *               
*****************************************************************               
         SPACE 1                                                                
EDTVAL   NTR1  ,                                                                
         USING PSND,R4                                                          
         L     R3,PSNCOMP                                                       
         ZIC   R6,PSNLEN                                                        
         CLI   PSNVSEP,C'*'        TEST FOR PRODUCT                             
         BE    EDTVAL2                                                          
*                                                                               
         GOTO1 CASHVAL,DMCB,(X'82',(R3)),(R6)                                   
         CLI   0(R1),X'FF'         TEST FOR ERROR                               
         BE    EDTVALR                                                          
         ZAP   DUB,4(8,R1)         EXTRACT INPUT VALUE                          
         GOTO1 TSTVAL,DUB          TEST FOR MAXIMUM VALUE                       
         B     EDTVALX                                                          
*                                                                               
EDTVAL2  GOTO1 CASHVAL,DMCB,(X'84',(R3)),(R6)                                   
         CLI   0(R1),X'FF'         TEST IF VALID NUMBER                         
         BE    EDTVALR                                                          
         ZAP   DUB,4(8,R1)         GET VALUE                                    
         GOTO1 TSTVAL,DUB          TEST SIZE OF VALUE                           
*                                                                               
         L     R4,PSNVAL           R4=A(VALUE ENTRY)                            
         L     R3,PSNCOMP          R3=A(VALUE)                                  
         ZIC   R6,PSNLEN                                                        
         GOTO1 CASHVAL,DMCB,(X'84',(R3)),(R6)                                   
         CLI   0(R1),X'FF'         TEST FOR VALUE                               
         BE    EDTVAL4             NO-LOOK FOR A WORKCODE                       
*                                                                               
         ZAP   WORK(16),4(8,R1)    MULTIPLICAND                                 
         GOTO1 TSTVAL,4(R1)                                                     
         MP    WORK(16),DUB        * MULTIPLIER                                 
         SRP   WORK(16),64-6,5     ROUND DOWN TO 2 DECIMALS                     
         ZAP   DUB,WORK(16)                                                     
         GOTO1 TSTVAL,DUB          TEST THE PRODUCT                             
         B     EDTVALX             ALL DONE                                     
*                                                                               
EDTVAL4  CLI   PSNLEN,2            MUST BE TWO CHARACTERS                       
         BL    EDTVALR                                                          
         CLI   PSNLEN,3            WORKCODE IS NO MORE THAN 3 CHARS             
         BH    EDTVALR                                                          
*                                                                               
         MVI   REFSW,C'Y'          NOTE WORKCODE REFERENCE                      
         XC    WORK,WORK                                                        
         MVC   WORK(2),0(R3)       EXTRACT WORKCODE(2)                          
         CLI   PSNLEN,2            TEST FOR SUFFIX                              
         BE    *+10                NO                                           
         MVC   WORK+2(1),2(R3)                                                  
*                                                                               
         L     R0,NESTENT          R0=LOOP COUNTER                              
         L     RE,AESTTAB          RE=A(ESTIMATE TABLE)                         
         L     RF,LESTTAB          RF=L'TABLE ENTRY                             
         USING ESTTABD,RE                                                       
*                                                                               
EDTVAL6  CLC   WORK(3),ESTWORKC    MATCH AGAINST TABLE                          
         BE    EDTVAL8                                                          
         AR    RE,RF                                                            
         BCT   R0,EDTVAL6                                                       
         B     EDTVALR             NOT IN TABLE                                 
*                                                                               
EDTVAL8  CLI   COMMSW,C'Y'         TEST COMMISSIONABLE FIELD EDIT               
         BNE   EDTVALR             NO-ONLY WANT ONE WORKCODE REFERENCE          
         CLI   EDTSW,0             CANNOT ADD TO PREVIOUS                       
         BE    EDTVAL9                                                          
         MVI   ERROR,SUPPLIED                                                   
         MVC   CONHEAD(L'INCONMSG),INCONMSG                                     
         B     EDTVALR                                                          
*                                                                               
EDTVAL9  CLI   PASS,1              TEST FOR FIRST PASS                          
         BE    EDTVALX                                                          
*                                                                               
         MVI   ELCODE,EDAELQ                                                    
         BAS   RE,GETELIO                                                       
         B     *+8                                                              
*                                                                               
EDTVAL9A BAS   RE,NEXTEL                                                        
         BE    *+14                                                             
         ZAP   DUB,=P'0'           RESULT IS ZERO                               
         B     EDTVALX                                                          
*                                                                               
         USING EDAELD,R6                                                        
         TM    EDATYPE,EDATWORK    IS THIS A WORKCODE?                          
         BZ    EDTVAL9A            NO, READ AGAIN                               
         CLC   EDAWORK,WORK        YES, IS IT THE ONE WE WANT?                  
         BNE   EDTVAL9A            NO, READ AGAIN                               
*                                                                               
EDVAL10  ZAP   WORK(16),EDACOMM    EXTRACT COMMISSIONABLE FIGURE                
         CLI   COMMSW,C'Y'         TEST FOR COMMISSIONABLE VALUE                
         BE    EDTVAL12            YES                                          
         ZAP   WORK(16),=P'0'                                                   
         CLI   EDALN,EDALNQ2       TEST FOR DOUBLE VALUE ELEM                   
         BL    *+10                NO                                           
         ZAP   WORK(16),EDANCOM    EXTRACT NON-COMMISSIONABLE                   
*                                                                               
EDTVAL12 MP    WORK(16),DUB        WORKCODE VALUE*CONSTANT                      
         SRP   WORK(16),64-4,5     ROUND DOWN TO TWO DECIMALS                   
         ZAP   DUB,WORK(16)        SET OUTPUT VALUE                             
*                                                                               
EDTVALX  B     XIT                                                              
*                                                                               
EDTVALR  B     EDTWCR                                                           
         DROP  RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO HANDLE SPECIAL (CALCULATED WORKCODES)                          
*                                                                               
         USING JBWCTABD,R3                                                      
         USING COLD,R4                                                          
SPECWC   NTR1                                                                   
         LA    R3,WCTABLE          GET WORKCODE TABLE                           
*                                                                               
SPECW02  GOTO1 AGETEST,DMCB,(RC),COLEST                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,LOOK             LET JOBBER GET CATEGORY TOTALS               
         CLI   JBWCODE,0           AT END OF TABLE ?                            
         BE    SPECWCX             YES, EXIT                                    
         BAS   RE,GETPER           GET PERCENTAGE FROM GETOPTS                  
*                                                                               
         USING JBLOCKD,R5                                                       
         L     R5,AIO2                                                          
         LH    R0,JBNROWS          NUMBER OF ENTRIES                            
         L     R6,JBACOLTB         COLUMN OUTPUT TABLE                          
*                                                                               
         USING JBCOLD,R6                                                        
SPECW04  CLI   JBCOLTYP,JBCOLTCT                                                
         BL    SPECW06                                                          
         CLI   JBCOLTYP,JBCOLTCF                                                
         BH    SPECW06                                                          
         CLC   JBCOLCAT,JBWCREF                                                 
         BE    SPECW08                                                          
*                                                                               
SPECW06  AH    R6,JBLCOL                                                        
         BCT   R0,SPECW04                                                       
         MVI   ERROR,BADCAT                                                     
         B     EDTWCR                                                           
*                                                                               
SPECW08  ZAP   WORK(16),JBCOLVAL   GET COLUMN VALUE                             
         MP    WORK(16),DUB        MULTIPLY BY PERCENTAGE                       
         SRP   WORK(16),64-6,5     SHIFT/ROUND TO 2 DECIMALS PLACES             
         ZAP   SETAMT,WORK(16)                                                  
*                                                                               
         USING EDAELD,R6                                                        
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   EDAEL,EDAELQ                                                     
         MVI   EDALN,EDALNQ1                                                    
         MVI   EDATYPE,EDATWORK                                                 
         MVC   EDAWORK,JBWCODE                                                  
         ZAP   EDACOMM,SETAMT                                                   
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),('EDAELQ',AIO),(3,EDATYPE)              
         GOTO1 ADDELEM                                                          
         GOTO1 WRITE                                                            
*                                                                               
         LA    R3,JBWCTABL(R3)                                                  
         B     SPECW02                                                          
*                                                                               
SPECWCX  B     XIT                                                              
         DROP  R3,R4,R5,R6                                                      
         EJECT                                                                  
* SUB-ROUTINE TO CHECK THE VALUE RETURNED BY CASHVAL AGAINST                    
* THE MAXIMUM--CALLED FROM EDTVAL ONLY                                          
* AT ENTRY, 0(8,R1) = A(VALUE TO CHECK)                                         
* IF ERROR, ROUTINE EXITS DIRECTLY TO EDTVALR                                   
*                                                                               
TSTVAL   CP    0(8,R1),MAXVAL      TEST HIGHER THAN MAXIMUM                     
         BH    EDTVALR                                                          
         CP    0(8,R1),MAXNEG      TEST LOWER THAN MAX NEGATIVE                 
         BL    EDTVALR                                                          
         BR    RE                                                               
         SPACE 2                                                                
MAXVAL   DC    PL8'99999999999'                                                 
MAXNEG   DC    PL8'-99999999999'                                                
         SPACE 2                                                                
* AT ENTRY, R2=A(SELECT FIELD HEADER)                                           
*                                                                               
SETLIN   ST    RE,SAVERE                                                        
         LA    R0,NDATAFLD                                                      
         LA    R1,ASEL                                                          
         ST    R2,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BAS   RE,BUMP                                                          
         BCT   R0,*-12                                                          
         ST    R2,ANEXTSEL                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO PERFORM A JOB LOOKUP--CALLED BY SPECWC                         
*                                                                               
         USING COLD,R4                                                          
         USING JBLOCKD,R5                                                       
LOOK     NTR1  ,                                                                
         L     R5,AIO2             USE IO2 AS JOBBLOCK AREA                     
         LR    RE,R5                                                            
         LA    RF,JBLOCKL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    LISTAR,LISTAR       BUILD A DUMMY FIELD HEADER                   
         MVC   LISTAR+8(1),COLEST  FOR THE ESTIMATE                             
         ZIC   R0,COLEST+1                                                      
         EDIT  (R0),(3,LISTAR+9),ALIGN=LEFT                                     
         AH    R0,=H'1'            FORM INPUT LENGTH                            
         STC   R0,LISTAR+5                                                      
         MVI   LISTAR,L'LISTAR                                                  
         GOTO1 VJOBCOL,DMCB,(1,LISTAR),WORK,ACOMFACS                            
         CLI   4(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                DUMP IF AN ERROR                             
*                                                                               
         MVC   JBAJOB,AJOB                                                      
         LA    RE,WORK                                                          
         ST    RE,JBACOLS                                                       
         MVC   JBACOM,ACOMFACS                                                  
         LA    RE,GOBLOCK                                                       
         ST    RE,JBAGOBLK                                                      
         MVC   JBAIO,AIO1                                                       
         MVC   JBGETOPT,GETOPT                                                  
         MVC   JBSELSCH,JOBSCH                                                  
         MVI   JBSELFUN,JBGETEST   EXTRACT VALUE FROM ESTIMATE                  
*                                                                               
         L     R1,ATIA                                                          
         ST    R1,JBACOLTB                                                      
         L     RE,=A(LENTIA)                                                    
         SRL   RE,1                DIVIDE THE 14K TIA IN HALF                   
         ST    RE,JBLCOLTB                                                      
         ST    RE,JBLOPVTB                                                      
         LA    R1,0(RE,R1)         POINT TO SECOND HALF OF TIA                  
         ST    R1,JBAOPVTB         USE FOR OPERAND VALUE TABLE                  
         XC    WCTABLE,WCTABLE                                                  
         LA    R1,WCTABLE                                                       
         ST    R1,JBAWCTB                                                       
         MVC   JBAWCTBL,=AL4(L'WCTABLE)                                         
         GOTO1 JOBBER,DMCB,JBLOCK                                               
         CLI   JBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LOOKX    B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
* SUB-ROUTINE TO GET ESTIMATE PERCENTAGE (BY WORKCODE) FROM GETOPT              
* R3 =A(WCTABLE)                                                                
*                                                                               
         USING JBWCTABD,R3                                                      
GETPER   NTR1                                                                   
         MVC   GOADM,DATAMGR       READ 'TO' JOB'S OPTIONS                      
         MVC   GOSELCUL,CUL                                                     
         MVC   GOSELCLI,CLICODE                                                 
         MVC   GOSELPRO,PRODCODE                                                
         MVC   GOSELJOB,JOBNUM                                                  
         MVC   GOSELWC,JBWCODE                                                  
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         ZAP   DUB,GOESTPER                                                     
         XC    GOSELWC,GOSELWC                                                  
*                                                                               
GETPERX  B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
*                                                                               
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         SPACE 1                                                                
YESXIT   CR    RB,RB               SET CC=EQ AND EXIT                           
         B     XIT                                                              
         SPACE 1                                                                
NOXIT    LTR   RB,RB               SET CC=NEQ AND EXIT                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
RELO     DC    A(0)                                                             
         EJECT                                                                  
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
SLUSH    DC    C'&&&&'                                                          
INCONMSG DC    C'** CANNOT REFER TO WORKCODE AND UPDATE PREVIOUS **'            
*                                                                               
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECTS ARE HIDDEN IN HERE                                        
         SPACE 3                                                                
*ACPROWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACGENBOTH                                                                      
*DDTWABLDD                                                                      
*ACGENFILE                                                                      
*FAFACTS                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE DDTWABLDD                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE ACPRO32COM                                                     
       ++INCLUDE DDPARSNIPD                                                     
         EJECT                                                                  
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060ACPRO43   09/12/02'                                      
         END                                                                    
