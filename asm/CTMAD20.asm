*          DATA SET CTMAD20    AT LEVEL 070 AS OF 08/05/97                      
*PHASE TA0C20A,*                                                                
*INCLUDE SCRUMPY                                                                
         TITLE 'TA0C20 - $MAD SCRIPT EXECUTOR'                                  
TA0C20   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0C20,RA,RR=R4                                                
*                                                                               
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
         USING CONTROLD,RC                                                      
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
         USING OVERD,R9                                                         
*                                                                               
         L     R1,=V(SCRUMPY)                                                   
         AR    R1,R4                                                            
         ST    R1,ASCRUMPY                                                      
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
         EJECT                                                                  
*************************************************************                   
*        MAIN CONTROL                                       *                   
*************************************************************                   
         SPACE 1                                                                
CONTROL  DS    0H                                                               
*                                                                               
         CLI   OVERMODE,C'S'       MODE START  => MAIN                          
         BE    CNTL010                                                          
         CLI   OVERMODE,C'M'       MODE MIDDLE => MAIN                          
         BE    CNTL010                                                          
         CLI   OVERMODE,C'E'       MODE END    => EXIT                          
         BE    CX                                                               
*                                                                               
CNTL010  BAS   RE,MAIN             CALL MAIN                                    
*                                                                               
CX       B     EXIT                AND EXIT                                     
         EJECT                                                                  
******************************************************************              
*        MAIN PROGRAM                                            *              
******************************************************************              
         SPACE 1                                                                
MAIN     NTR1                                                                   
         CLC   PCACTION,=Y(ACUPSCRP)    TEST UPLOAD                             
         BNE   MAIN010                                                          
*                                                                               
         CLI   OVERMODE,C'S'       IF UPLOAD START                              
         BNE   MAIN005                                                          
*                                                                               
         XC    OBJCOUNT,OBJCOUNT   CLEAR SOME SAVED VARIABLES                   
*                                                                               
MAIN005  BAS   RE,UPLOAD                                                        
         B     MAINX                                                            
*                                                                               
MAIN010  CLC   PCACTION,=Y(ACDNSCRP)    TEST DOWNLOAD                           
         BNE   *+12                                                             
         BAS   RE,DOWNLOAD                                                      
         B     MAINX                                                            
*                                                                               
         CLC   PCACTION,=Y(ACRNSCRP)    TEST EXECUTE SCRIPT                     
         BNE   *+12                                                             
         BAS   RE,SEXEC                                                         
         B     MAINX                                                            
*                                                                               
         CLC   PCACTION,=Y(ACSCRIPT)    TEST ONE FRAME SCRIPT                   
         BNE   *+12                                                             
         BAS   RE,ONEFRAME                                                      
         B     MAINX                                                            
*                                                                               
         CLC   PCACTION,=Y(ACSCRSTA)    TEST SCRIPT STATUS ENQ                  
         BNE   *+12                                                             
         BAS   RE,STATUS                                                        
         B     MAINX                                                            
*                                                                               
MAINX    B     XIT                                                              
         EJECT                                                                  
******************************************************************              
*        MAIN SCRIPT UPLOAD PROGRAM                              *              
******************************************************************              
         SPACE 1                                                                
UPLOAD   NTR1                                                                   
         CLI   OVERMODE,C'S'       OPEN TEMP IF MODESTART                       
         BNE   UPL015                                                           
         XC    AFID,AFID           FILE ID IS NULL UNLESS REQUESTED             
*                                                                               
         GOTO1 GETITEM             GET AN ITEM                                  
         BNE   EXIT                                                             
         CLI   EIFFLAG,C'Y'        IF END OF FRAME JUST OPEN FILE               
         BE    UPL010                                                           
*                                                                               
         CLC   TYPENUM,=A(ITWRKFID)                                             
         BNE   UPL005                                                           
         CLC   DATALEN,=F'7'       DATA LEN MUST BE 7 ID(6)+TYPE                
         BNE   ERRINV                                                           
*                                                                               
         L     RE,ADATA            SET FILE ID IN DUB                           
         MVC   DUB(7),0(RE)                                                     
         LA    RF,DUB                                                           
         ST    RF,AFID                                                          
         B     UPL010                                                           
*                                                                               
UPL005   MVC   AINPUT,AINFRM       REWIND GETITEM IF NOT FILE ID                
         MVC   INPLEFT,MDFRMSIZ                                                 
         MVI   EIFFLAG,C'N'                                                     
*                                                                               
UPL010   GOTO1 WRKCRE,DMCB,AFID    CREATE WORKER FILE                           
         BNE   EXIT                                                             
         B     UPL020                                                           
*                                                                               
UPL015   GOTO1 WRKREPT             REOPEN FOR PUT                               
*                                                                               
UPL020   BAS   RE,FILLTMP          FILL WRKF FILE WITH MAD OBJECTS              
*                                                                               
         LA    RF,TMPDATA          SEND CONFIRMATION OBJECT                     
         XC    0(16,RF),0(RF)                                                   
         L     R1,OBJCOUNT         NUMBER OF DATA OBJECTS RECEIVED              
         EDIT  (R1),(6,0(RF)),FILL=0                                            
         GOTO1 PUTITEM,DMCB,ITSCCONF,6,TMPDATA                                  
*                                                                               
         CLI   OVERMODE,C'S'       IF MODESTART                                 
         BNE   UPLX                                                             
*                                  RETURN FILE NUMBER OBJECT TO PC              
         GOTO1 HEXOUT,DMCB,WRKFILNO,FULL,2                                      
         GOTO1 PUTITEM,DMCB,A(ITWRKNUM),4,FULL                                  
         BNE   EXIT                                                             
         MVC   SVWRKNUM,WRKFILNO   SAVE NUMBER                                  
*                                                                               
UPLX     CLI   PCLAST,C'Y'         TEST LAST FRAME                              
         BNE   UPLXX                                                            
         GOTO1 WRKCLOS             CLOSE FILE                                   
         B     XIT                                                              
UPLXX    GOTO1 WRKCLOT             CLOSE FILE TEMPORARILY                       
         B     XIT                                                              
         EJECT                                                                  
******************************************************************              
*        MAIN SCRIPT DOWNLOAD PROGRAM                            *              
******************************************************************              
         SPACE 1                                                                
DOWNLOAD NTR1                                                                   
         CLI   OVERMODE,C'S'       OPEN TEMP IF MODESTART                       
         BNE   DNL020                                                           
*                                                                               
         GOTO1 GETITEM             TEST FOR FILE NUMBER OBJECT                  
         BNE   EXIT                                                             
         CLI   EIFFLAG,C'Y'                                                     
         BNE   DNL005                                                           
         OC    SVWRKNUM,SVWRKNUM   TEST SAVE NUMBER                             
         BZ    EXIT                                                             
         MVC   WRKFILNO,SVWRKNUM                                                
         B     DNL010                                                           
*                                                                               
DNL005   CLC   TYPENUM,=A(ITWRKNUM)                                             
         BNE   DNL010                                                           
*                                                                               
         L     R2,DATALEN          THEN SET WORKER FILE NUMBER                  
         GOTO1 HEXIN,DMCB,ADATA,HALF,4                                          
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRINV                                                           
         MVC   WRKFILNO,HALF                                                    
*                                                                               
DNL010   GOTO1 WRKLOC              LOCATE WORKER FILE                           
         BNE   EXIT                                                             
         L     R1,ATMPBUF                                                       
         L     R1,W_RECS-W_RECD(R1)                                             
         LA    RF,TMPDATA          NUMBER OF DOWNLOAD RECS                      
         EDIT  (R1),(7,0(RF)),FILL=0                                            
         GOTO1 PUTITEM,DMCB,ITSCRDOW,7,TMPDATA                                  
         BNE   EXIT                                                             
         XC    TMPTYPE,TMPTYPE     CLEAR CONTINUE DATA                          
         B     DNL030                                                           
*                                                                               
DNL020   GOTO1 WRKREGT             REOPEN WORKER FOR GET                        
*                                                                               
DNL030   BAS   RE,FILLFRM          FILL FRAME WITH MAD OBJECTS                  
*                                                                               
DNLX     B     XIT                                                              
         EJECT                                                                  
******************************************************************              
*        MAIN SCRIPT EXEC PROGRAM                                *              
******************************************************************              
         SPACE 1                                                                
SEXEC    NTR1                                                                   
*                                                                               
         GOTO1 GETITEM             EXEC MUST HAVE FILE NUMBER                   
         BNE   EXIT                                                             
         CLI   EIFFLAG,C'Y'                                                     
         BNE   PS05                                                             
*                                                                               
         OC    SVWRKNUM,SVWRKNUM   TEST SAVE NUMBER                             
         BZ    ERRNFIL                                                          
         MVC   WRKFILNO,SVWRKNUM                                                
         MVC   HALF,SVWRKNUM                                                    
         B     PS10                                                             
*                                                                               
PS05     CLC   TYPENUM,=A(ITWRKNUM)                                             
         BNE   ERRFOBJ                                                          
         L     R2,DATALEN          SET WORKER FILE NUMBER                       
         GOTO1 HEXIN,DMCB,ADATA,HALF,4                                          
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRINV                                                           
*                                                                               
PS10     XC    WORK,WORK           BUILD KEY IN WORK                            
         MVC   WORK+UKFILENO-UKRECD(2),HALF                                     
         OI    WORK+UKFLAG-UKRECD,X'80'                                         
         MVC   WORK(2),SIGNON2                                                  
*                                                                               
         GOTO1 PROTOFF             DISSABLE STORAGE PROTECTION                  
*                                                                               
         GOTO1 ASCRUMPY,DMCB,ACOMFACS,ATIA,0,WORK,0                             
*                                                                               
         GOTO1 PROTON              RESTORE STORAGE PROTECTION                   
*                                                                               
         LA    RF,TMPDATA          SCRIPT RETURN CODES                          
         XC    0(16,RF),0(RF)                                                   
         LR    RE,R1                                                            
         LH    R1,16(RE)           ERROR                                        
         EDIT  (R1),(6,0(RF)),FILL=0                                            
         MVI   6(RF),C','                                                       
         LH    R1,18(RE)           DISPLACEMENT                                 
         EDIT  (R1),(6,7(RF)),FILL=0                                            
         GOTO1 PUTITEM,DMCB,ITSCRETN,13,TMPDATA                                 
         BNE   EXIT                                                             
*                                                                               
SEXX     MVI   MDLAST,C'Y'                                                      
         B     XIT                                                              
         EJECT                                                                  
*************************************************************                   
*        MAIN ONE FRAME UPLOAD/EXECUTE/DOWNLOAD PROG        *                   
*************************************************************                   
         SPACE 1                                                                
ONEFRAME NTR1                                                                   
         CLI   OVERMODE,C'S'       OPEN TEMP IF MODESTART                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,NOPTWA           NOP TEMPSTR I/O                              
         ST    R1,READTWA                                                       
         ST    R1,WRTTWA                                                        
*                                                                               
         GOTO1 TMPOPEN,DMCB,=C'PUT'                                             
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,FILLTMP          FILL TEMP FILE WITH MAD OBJECTS              
         CLI   MDLAST,C'Y'                                                      
         BE    *+6                                                              
         DC    H'0'                MUST BE ONLY ONE FRAME                       
*                                                                               
         BAS   RE,SEXEC            EXECUTE SCRIPT                               
*                                                                               
         GOTO1 TMPCLOSE            CLOSE TEMP FILE                              
         BNE   EXIT                                                             
*                                                                               
         MVC   TMPTWA,CURRTWA      SAVE TWA NUMBER OF END OF TMPFILE            
         MVC   TMPDNEXT,DNEXTTMP   DISP WITHIN LAST TWA                         
         GOTO1 TMPOPEN,DMCB,=C'GET',L'TMPAREA                                   
         BNE   EXIT                                                             
         XC    TMPTYPE,TMPTYPE     CLEAR CONTINUE DATA                          
*                                                                               
         BAS   RE,FILLFRM          FILL FRAME WITH MAD OBJECTS                  
*                                                                               
ONEXX    B     XIT                                                              
         EJECT                                                                  
******************************************************************              
*        MAIN SCRIPT STATUS ENQUIRY                              *              
******************************************************************              
         SPACE 1                                                                
STATUS   NTR1                                                                   
*                                                                               
         GOTO1 GETITEM             TEST FOR FILE NUMBER OBJECT                  
         BNE   EXIT                                                             
         CLI   EIFFLAG,C'Y'                                                     
         BNE   STAT05                                                           
*                                                                               
         OC    SVWRKNUM,SVWRKNUM   TEST SAVE NUMBER                             
         BZ    EXIT                                                             
         MVC   WRKFILNO,SVWRKNUM                                                
         B     STAT010                                                          
*                                                                               
STAT05   CLC   TYPENUM,=A(ITWRKNUM)                                             
         BNE   STAT010                                                          
*                                                                               
         L     R2,DATALEN          THEN SET WORKER FILE NUMBER                  
         GOTO1 HEXIN,DMCB,ADATA,HALF,4                                          
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRINV                                                           
         MVC   WRKFILNO,HALF                                                    
*                                                                               
STAT010  GOTO1 WRKLOC              LOCATE WORKER FILE                           
         BNE   EXIT                                                             
         XC    TMPTYPE,TMPTYPE     CLEAR CONTINUE DATA                          
*                                                                               
         LA    R1,ELEMENT          GET STATUS INTO BYTE                         
         MVC   BYTE,UKSTAT-UKRECD(R1)                                           
*                                                                               
         MVC   FULL,=C'DELD'       TEST DELETED FIRST                           
         TM    BYTE,X'04'                                                       
         BO    STAT050                                                          
         MVC   FULL,=C'HOLD'       TEST HOLD NEXT                               
         TM    BYTE,X'40'                                                       
         BO    STAT050                                                          
         MVC   FULL,=C'RING'       TEST RUNNING                                 
         CLI   BYTE,X'83'                                                       
         BE    STAT050                                                          
         MVC   FULL,=C'SING'       TEST SENDING                                 
         CLI   BYTE,X'82'                                                       
         BE    STAT050                                                          
         MVC   FULL,=C'PROC'       TEST PROCESSED                               
         CLI   BYTE,X'A0'                                                       
         BE    STAT050                                                          
         MVC   FULL,=C'SENT'       TEST SENT                                    
         TM    BYTE,X'10'                                                       
         BO    STAT050                                                          
         MVC   FULL,=C'ACTV'       TEST ACTV                                    
         TM    BYTE,X'80'                                                       
         BO    STAT050                                                          
         MVC   FULL,=C'UNKN'       ELSE UNKNOWN                                 
*                                                                               
STAT050  LA    RF,TMPDATA          SCRIPT RETURN CODES                          
         XC    0(16,RF),0(RF)                                                   
         MVC   0(4,RF),FULL                                                     
         GOTO1 PUTITEM,DMCB,ITSCRSTA,4,TMPDATA                                  
         BNE   EXIT                                                             
         MVI   MDLAST,C'Y'                                                      
*                                                                               
STATX    B     XIT                                                              
         EJECT                                                                  
*************************************************************                   
* THIS ROUTINE FILLS THE FRAME WITH OBJECTS FROM TEMPSTR    *                   
*************************************************************                   
         SPACE 1                                                                
FILLFRM  NTR1                                                                   
         OC    TMPTYPE,TMPTYPE     IF CONTINUE TMPAREA EXISTS THEN CONT         
         BNZ   FF20                                                             
*                                  GET NEXT ITEM FROM TEMP FILE                 
FF10     GOTO1 WRKGET,DMCB,TMPHDR                                               
         BNE   EXIT                                                             
*                                                                               
         CLC   TMPHDR(8),=C'*EOFEOF*'                                           
         BNE   FF15                END OF TEMP FILE                             
*                                                                               
         GOTO1 WRKSENT             MARK REPORT SENT                             
*                                                                               
         MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR AND DONE            
         B     FFX                                                              
*                                                                               
FF15     SR    R1,R1                                                            
         ICM   R1,3,TMPHDR                                                      
         SH    R1,=H'8'                                                         
         ST    R1,TMPLEN                                                        
         MVC   TYPENUM,TMPAREA                                                  
*                                                                               
         CLC   TYPENUM,=A(ITSCRIPT) SCRIPT DEFINITION                           
         BE    FF16                                                             
         CLC   TYPENUM,=A(ITSCRHDR) SCRIPT DEFINITION (EXTENDED)                
         BE    FF16                                                             
         B     FF20                                                             
*                                                                               
FF16     LA    R1,TMPDATA          SET UP A SETDEF CALL                         
         ST    R1,ADATA                                                         
         SR    R1,R1                                                            
         ICM   R1,3,TMPHDR                                                      
         ST    R1,DATALEN                                                       
         BAS   RE,SETDEF                                                        
*                                                                               
FF20     ICM   R2,15,TMPTYPE       PUT MAD OBJECT TO OUTPUT                     
         L     R3,TMPLEN                                                        
*                                                                               
         CLI   SCRFORMT,C'I'       TEST INSERT FORMAT                           
         BNE   FF30                                                             
         CLC   TMPTYPE,=A(ITSCDATA)    SCRIPT DATA ONLY                         
         BNE   FF30                                                             
*                                                                               
         SR    R1,R1               R1=LEN OF OUTPUT                             
         ICM   R1,15,SCROPLEN                                                   
         SR    RF,RF               RF=LEN OF HEADER                             
         ICM   RF,15,SCRHDLEN                                                   
         LA    RF,TMPDATA(RF)      RF=A(OUTPUT)                                 
         BCTR  R1,0                                                             
         EX    R1,*+8              EX TEST FOR ZERO OUTPUT                      
         B     *+10                                                             
         OC    0(0,RF),0(RF)       IGNORE THIS OBJECT IF ZERO                   
         BZ    FF10                                                             
         EX    R1,*+8              EX TEST FOR SPACES OUTPUT                    
         B     *+10                                                             
         CLC   0(0,RF),SPACES      IGNORE THIS IF SPACES                        
         BE    FF10                                                             
*                                                                               
         ICM   R3,15,SCRHDLEN      SCRIP OUTPUT IS HDR+O/P                      
         ICM   R1,15,SCROPLEN                                                   
         AR    R3,R1                                                            
*                                                                               
FF30     GOTO1 PUTITEM,DMCB,(R2),(R3),TMPDATA                                   
         BNE   EXIT                                                             
*                                                                               
         CLI   EOFFLAG,C'Y'        IF NOT END OF FRAME                          
         BNE   FF10                THEN GO BACK                                 
*                                                                               
FFX      B     XIT                                                              
         EJECT                                                                  
*************************************************************                   
* THIS ROUTINE FILLS TEMP FILE WITH OBJECTS FROM THE FRAME  *                   
*************************************************************                   
         SPACE 1                                                                
FILLTMP  NTR1                                                                   
*                                                                               
FT10     GOTO1 GETITEM             GET MAD OBJECT FROM SCREEN                   
         BNE   ERRCLS                                                           
*                                                                               
         CLI   EIFFLAG,C'Y'        IF END OF FRAME THEN DONE FOR                
         BE    FTX                 THIS TRANSACTION                             
*                                                                               
         CLC   TYPENUM,=A(ITSCDATA) MODIFY SCRIPT DATA OBJECTS                  
         BNE   FT11                                                             
*                                                                               
         L     R1,OBJCOUNT         BUMP NO OF OBJECTS                           
         LA    R1,1(R1)                                                         
         ST    R1,OBJCOUNT                                                      
*                                                                               
FT11     MVC   TMPTYPE,TYPENUM                                                  
*                                                                               
         CLC   TYPENUM,=A(ITSCRIPT) SCRIPT DEFINITION                           
         BNE   *+8                                                              
         BAS   RE,SETDEF                                                        
         CLC   TYPENUM,=A(ITSCRHDR) SCRIPT DEFINITION (EXTENDED)                
         BNE   *+8                                                              
         BAS   RE,SETDEF                                                        
*                                                                               
         L     RE,ADATA            TYPE AND DATA FROM GETITEM                   
         LA    R2,TMPDATA          R2=A(TEMP AREA)                              
         L     RF,DATALEN                                                       
*                                                                               
         CLI   SCRFORMT,C'I'       TEST FOR INSERT FORMAT                       
         BNE   FT20                                                             
         CLC   TYPENUM,=A(ITSCDATA) MODIFY SCRIPT DATA OBJECTS                  
         BNE   FT20                                                             
*                                                                               
         OC    SCRIPDEF,SCRIPDEF   MUST HAVE A SCRIPT DEFINITION                
         BZ    ERR01                                                            
*                                                                               
         ICM   R1,15,SCRHDLEN      COPY HEADER TO TEMP AREA                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RE)                                                    
         LA    R2,1(R1,R2)         INDEX PAST HEADER                            
*                                                                               
         LA    RE,1(R1,RE)         INDEX PAST HEADER                            
         SR    RF,R1               SUB HDRLEN FROM LEN                          
         BCTR  RF,0                                                             
*                                                                               
         ICM   R1,15,SCROPLEN      INCREASE DATALEN BY O/P AREA                 
         A     R1,DATALEN                                                       
         ST    R1,DATALEN                                                       
         ICM   R1,15,SCROPLEN      COPY O/P SPACES TO TEMP AREA                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SPACES                                                   
         LA    R2,1(R1,R2)         INDEX PAST HEADER                            
*                                                                               
FT20     LR    R3,RF               MOVE THE REST                                
         MVCL  R2,RE                                                            
*                                                                               
         CLI   PCLAST,C'Y'         LOOK FOR EOD                                 
         BNE   *+8                                                              
         MVI   MDLAST,C'Y'         FLAG LAST DATA                               
*                                                                               
         L     R3,DATALEN          ADD RECORD TO TEMP FILE                      
         LA    R3,8(R3)                                                         
         XC    TMPHDR,TMPHDR                                                    
         STH   R3,TMPHDR                                                        
         GOTO1 WRKPUT,DMCB,TMPHDR                                               
         BNE   ERRCLS                                                           
         B     FT10                LOOP BACK                                    
*                                                                               
FTX      B     XIT                                                              
         EJECT                                                                  
***********************************************************                     
*        HANDLE SCRIPT DEFINITION OBJECT                  *                     
***********************************************************                     
         SPACE 1                                                                
*                                                                               
*        SCRIPT DEF DATA CL2'LENHDR'CL2'LENOP'CL8'NAME'                         
*                                                                               
SETDEF   NTR1                                                                   
*                                                                               
         CLC   TYPENUM,=A(ITSCRIPT) SCRIPT DEFINITION                           
         BE    SETD01                                                           
         CLC   TYPENUM,=A(ITSCRHDR) SCRIPT DEFINITION (EXTENDED)                
         BE    SETD02                                                           
         DC    H'0'                                                             
*                                                                               
SETD01   MVC   SCRIPTID,SPACES     START WITH SPACES                            
         L     R1,DATALEN                                                       
         L     RE,ADATA                                                         
         SH    R1,=H'5'            LEN=LEN-4 -1 FOR EX                          
         EX    R1,*+8              MOVE NAME INTO SCRIPTID                      
         B     *+10                                                             
         MVC   SCRIPTID(0),4(RE)   SET NAME                                     
         PACK  DUB,0(2,RE)                                                      
         CVB   R1,DUB                                                           
         STCM  R1,15,SCRHDLEN      SET HDR LEN                                  
         PACK  DUB,2(2,RE)                                                      
         CVB   R1,DUB                                                           
         STCM  R1,15,SCROPLEN      SET O/P LEN                                  
*                                                                               
         MVI   SCRFORMT,C'I'       FORMAT MUST BE INSERT                        
         B     XIT                                                              
*                                                                               
SETD02   L     RE,ADATA            EXTENDED SCRIPT ID                           
         MVC   SCRIPTID,6(RE)                                                   
         PACK  DUB,26(5,RE)                                                     
         CVB   R1,DUB                                                           
         STCM  R1,15,SCRHDLEN      SET HDR LEN                                  
         PACK  DUB,36(5,RE)                                                     
         CVB   R1,DUB                                                           
         STCM  R1,15,SCROPLEN      SET O/P LEN                                  
         MVC   SCRFORMT,14(RE)                                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************                     
*        EXITS     2006 OBJECT AND EXIT  000000,000000    *                     
*        SCRIPT    2007 OBJECT AND EXIT  000000,000000    *                     
***********************************************************                     
         SPACE 1                                                                
OKBYE    LA    R1,0                RETURN 000000,NNNNNN                         
         B     ERRX                                                             
ERR01    LA    R1,1                NO SCRIPT DEFINITION RECEIVED                
         B     ERRX                                                             
*                                                                               
ERRX     LA    RF,TMPDATA                                                       
         XC    0(16,RF),0(RF)                                                   
         EDIT  (R1),(6,0(RF)),FILL=0                                            
         MVI   6(RF),C','                                                       
         EDIT  (B4,OBJCOUNT),(6,7(RF)),FILL=0                                   
         GOTO1 PUTITEM,DMCB,ITSCCONF,13,TMPDATA                                 
         B     EXIT                                                             
*                                                                               
ERRFOBJ  MVC   APPLERR,=H'1'       INVALID FILE NUMBER OBJECT TYPE              
         B     ERROBJ                                                           
*                                                                               
ERRNFIL  MVC   APPLERR,=H'2'       NO FILE NUMBER SAVED                         
         B     ERROBJ                                                           
*                                  RETURN APPL ERRORS IN ERR OBJECT             
ERROBJ   GOTO1 HEXOUT,DMCB,APPLERR,FULL,2                                       
         GOTO1 PUTITEM,DMCB,A(ITGENERR),4,FULL                                  
         BNE   EXIT                                                             
         MVI   MDLAST,C'Y'                                                      
         B     EXIT                                                             
*                                                                               
ERRINV   MVC   MDACTION,=Y(INVITEM)                                             
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
ERRCLS   GOTO1 WRKCLOE             ERROR CLOSE FILE                             
*                                                                               
EXIT     L     RD,SAVEDRD          RESTORE RD                                   
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
SPACES   DC    80C' '                                                           
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        NEXT INPUT REC                                     *                   
*************************************************************                   
         DS    0H                                                               
         USING *,RB                                                             
NEXTIN   NTR1  BASE=(RF)           USE NEW BASE                                 
         L     RC,4(R1)            RESTORE WORK                                 
         L     R2,8(R1)            RESTORE POINTER                              
         L     R9,12(R1)           RESTORE SAVED                                
*                                                                               
NXT010   SR    R3,R3               NEXT RECORD                                  
         ICM   R3,3,0(R2)          GET LEN (NOT INCLUDING 2 FOR LEN)            
         SH    R3,=H'4'            ADJUST FOR LEN OF OBJECT ID                  
         L     RE,ATIA                                                          
         LR    RF,R3                                                            
         LA    R2,6(R2)            POINT PAST LENGTH + OBJECT ID                
         MVCL  R2,RE               MOVE OBJECT FROM TIA OVER TWA                
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,0(R2)          GET RECORD LEN                               
*                                                                               
         ICM   R0,3,0(R2)          GET RECORD LEN                               
         BZ    NEXTREC             ZERO MUST BE NEXT TWA                        
         ST    R2,NXPARMS+8        NEXT POINTER                                 
*                                                                               
         CLC   2(4,R2),=A(ITEOD)        LOOK FOR EOD                            
         BE    NEXTEOD                                                          
         CLC   2(4,R2),=A(ITSCDATA)     LOOK FOR DATA                           
         BNE   NXT010                                                           
*                                                                               
*                                                                               
NTX020   L     R0,ATIA             GET 'TO' ADDR                                
         LA    R1,4095             SET 'TO' LEN = 4095                          
         SR    R3,R3                                                            
         ICM   R3,3,0(R2)          GET FROM LENGTH                              
         SH    R3,=H'4'            ADJUST FOR LEN OF OBJECT ID                  
         LA    R2,6(R2)            SET MOVE FROM ADDRESS                        
         MVCL  R0,R2                                                            
         B     NXTXIT              AND RETURN                                   
*                                                                               
NEXTEOD  MVI   NXPARMS,X'80'       FLAG EOF                                     
*                                                                               
NEXTREC  EQU   *                   ANOTHER TEMPSTR!                             
         GOTO1 WRTTWA,DMCB,ATMPBUF,CURRTWA                                      
         BNE   NXTXIT                                                           
         TM    NXPARMS,X'80'       EXIT IF EOD                                  
         BO    NXTXIT                                                           
*                                                                               
NXT050   SR    R1,R1               BUMP TWA NUMBER                              
         IC    R1,CURRTWA                                                       
         LA    R1,1(R1)                                                         
         STC   R1,CURRTWA                                                       
         CLC   CURRTWA,=F'5'       SKIP                                         
         BE    NXT050                                                           
         CLC   CURRTWA,=F'11'      FINNISH                                      
         BE    NXTXIT                                                           
*                                                                               
         GOTO1 READTWA,DMCB,ATMPBUF,CURRTWA                                     
         BNE   NXTXIT                                                           
         L     R2,ATMPBUF          R2=A(TMPBUF)                                 
         B     NXT010                                                           
*                                                                               
NXTXIT   LA    R1,NXPARMS          SET COND-CODE                                
         CLI   0(R1),0                                                          
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        NEXT OUTPUT REC                                    *                   
*************************************************************                   
         SPACE 1                                                                
NEXTOUT  BR    RE                  PISS OFF                                     
NOPTWA   BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        OVERLAY SAVED STORAGE                              *                   
*************************************************************                   
         SPACE 1                                                                
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
AFID     DS    A                   FILE ID OVERIDE OR ZERO                      
TMPTWA   DS    F                   END OF TEMP FILE TWA NUMBER                  
TMPDNEXT DS    F                   END OF TEMP FILE DISPLACEMENT                
TMPLEN   DS    F                   LENGTH OF TMPAREA (FROM WRKGET)              
NXPARMS  DS    6F                  PLIST FOR NEXTREC                            
ASCRUMPY DS    A                                                                
*                                                                               
         DS    0F                                                               
SCRIPDEF DS    0CL17               SPACE FOR SCRIPT DEFINITION                  
SCRIPTID DS    CL8                 SPACE FOR SCRIPT NAME                        
SCRHDLEN DS    F                   SCRIPT HEADER LEN                            
SCROPLEN DS    F                   SCRIPT OUTPUT DATA LEN                       
SCRFORMT DS    X                   SCRIPT DATA FORMAT                           
*                                                                               
OBJCOUNT DS    F                   COUNT OF DATA OBJECTS                        
*                                                                               
SVWRKNUM DS    F                   SAVED WORKER FILE NUMBER                     
*                                                                               
SCERR    DS    H                   SCRIP ERROR RETURN                           
SCDIS    DS    H                   SCRIPT ERROR DISPLACEMENT                    
*                                                                               
TMPHDR   DS    F                   RECORD HEADER                                
TMPAREA  DS    0XL1536             AREA TO READ TEMP FILE RECORD                
TMPTYPE  DS    XL4                 MAD OBJECT TYPE                              
TMPDATA  DS    XL1532              MAD OBJECT DATA                              
*                                                                               
MSGBUFF  DS    1536C                                                            
         EJECT                                                                  
* CTMADWORKD                                                                    
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
* CTMADEQUS                                                                     
       ++INCLUDE CTMADEQUS                                                      
         EJECT                                                                  
* CTMADDSECT                                                                    
       ++INCLUDE CTMADDSECT                                                     
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
* FASCRIPTD                                                                     
       ++INCLUDE FASCRIPTD                                                      
* DMWRKFK                                                                       
       ++INCLUDE DMWRKFK                                                        
* DMWRKFD                                                                       
       ++INCLUDE DMWRKFD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'070CTMAD20   08/05/97'                                      
         END                                                                    
