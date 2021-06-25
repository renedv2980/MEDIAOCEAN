*          DATA SET CTMAD01    AT LEVEL 015 AS OF 07/20/20                      
*PHASE TA0C01A                                                                  
*INCLUDE REFORM                                                                 
*INCLUDE GETBOOK                                                                
         TITLE 'TA0C01 - $MAD FREE FORMAT REPORT DOWNLOAD'                      
TA0C01   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0C01,RA,RR=R4                                                
*                                                                               
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
         L     R6,ATMPBUF          R6 = TEMPORARY STORAGE BUFFER                
         USING TEMPD,R6                                                         
*                                                                               
         L     R1,=V(REFORM)       RELOCATE EXTERNALS                           
         AR    R1,R4                                                            
         ST    R1,VREFORM                                                       
         L     R1,=A(REFAREA-TEMPD)                                             
         AR    R1,R6                                                            
         ST    R1,AREFAREA                                                      
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
                                                                                
***********************************************************************         
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
***********************************************************************         
MAIN     DS    0H                                                               
         BAS   RE,INIT             INITIALIZE OVERLAY                           
*                                                                               
         CLI   OVERMODE,C'S'       ANY ACTION START SHOULD CLEAR THIS           
         BNE   *+8                                                              
         MVI   SENTFLAG,C'N'                                                    
*                                                                               
         CLC   PCACTION,=Y(ACSENREP)  STATUS CHANGE                             
         BE    M30                                                              
*                                                                               
         CLI   OVERMODE,C'S'       IF MODE IS START                             
         BNE   M10                                                              
         BAS   RE,PROCSTRT         THEN CALL PROCSTRT                           
         B     MX                                                               
*                                                                               
M10      CLI   OVERMODE,C'M'       ELSE IF MODE IS MIDDLE                       
         BNE   M20                                                              
         BAS   RE,PROCMID          THEN CALL PROCMID                            
         B     MX                                                               
*                                                                               
M20      BAS   RE,PROCEND          ELSE CALL PROCEND                            
         B     MX                                                               
*                                                                               
M30      BAS   RE,PROCMARK         MARK REPORT AS SENT                          
*                                                                               
MX       B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE INITIALIZES VARIABLES IN THIS OVERLAY THAT NEED                  
* INITIALIZATION.                                                               
***********************************************************************         
INIT     NTR1                                                                   
*                                                                               
* INITIALIZE LOCAL WORKING STORAGE                                              
*                                                                               
IN10     MVI   FLREAD,C'N'         SET FIRST PLINE OF TRANS NOT READ            
*                                                                               
         GOTO1 INITBUF             INITIALIZE PQ BUFFER                         
         BNE   EXIT                                                             
*                                                                               
INX      B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE PROCESSES THE START MODE.  IT VALIDATES THE PQ ID PASSED         
* BY ALINK.  IT THEN RETURNS A DETAILED REPORT SUMMARY OBJECT FOLLOWED          
* BY THE FIRST FRAME FULL OF REPORT DATA OBJECTS.                               
***********************************************************************         
PROCSTRT NTR1                                                                   
         GOTO1 VALSPID             VALIDATE SPOOL ID                            
         BNE   EXIT                                                             
         MVI   NEWLSENT,0                                                       
*                                  VALIDATE REPORT                              
         GOTO1 GETNDX,DMCB,SIGNON2,REPSUBID,REPREPNO                            
         BE    PS10                                                             
         CLI   PQEOF,C'Y'          EOF - REPORT NOT FOUND                       
         BE    ERRRNF                                                           
         B     EXIT                DISK ERROR                                   
*                                                                               
PS10     GOTO1 GETATTR,DMCB,0      SAVE REPORT ATTRIBUTES                       
         BNE   EXIT                                                             
*                                                                               
         GOTO1 GETFACT,DMCB,(X'80',0),F#SESPR  PROTECT THE SESION               
*                                                                               
         LA    R5,INQUIRY          TEST IF VALID REPORT FOR DOWN LOAD           
         USING INQUIRYD,R5                                                      
         GOTO1 HEXIN,DMCB,IQATTR,BYTE,2                                         
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRJCL                                                           
         TM    BYTE,PQATJOBI       ERROR IF JCL IN REPORT                       
         BO    ERRJCL                                                           
         TM    BYTE,PQATERR        ERROR IF ERROR REPORT                        
         BO    ERRERROR                                                         
*NOP*    TM    BYTE,PQATPW         ERROR IF SECURE REPORT                       
*NOP*    BO    ERRRID                                                           
         GOTO1 HEXIN,DMCB,IQSTAT,BYTE,2                                         
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRJCL                                                           
         CLI   IQCLASS,C'G'        ERROR IF CLASS G                             
         BE    ERRRID                                                           
         GOTO1 HEXIN,DMCB,IQTYPE,BYTE,2                                         
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRJCL                                                           
         TM    BYTE,PQTYSQL                                                     
         BZ    *+14                                                             
         MVI   TYPE,C'S'           FLAG SQL REPORT                              
         MVC   SQREFID,IQDESC+6                                                 
*                                                                               
         CLC   PCACTION,=Y(ACDISALL)  DISALL GETS TOTAL INQ                     
         BE    *+14                                                             
         CLC   PCACTION,=Y(ACDISREP)  DISREP GETS TOTAL INQ                     
         BNE   PS20                                                             
*                                                                               
         SR    RF,RF               FIELD IS IQTOTLEN+10+STRDAT                  
         CLC   PCVRS,=H'330'       FOR VERSION 330                              
         BL    *+8                                                              
         ICM   RF,3,IQSTRLEN                                                    
         LA    RF,IQTOTLEN(RF)                                                  
         GOTO1 PUTITEM,DMCB,ITTOTINQ,(RF),INQUIRY                               
         BNE   EXIT                                                             
         B     PS30                                                             
         DROP  R5                                                               
*                                                                               
PS20     GOTO1 PUTITEM,DMCB,ITDETINQ,IQDETLEN,INQUIRY                           
         BNE   EXIT                                                             
*                                                                               
PS30     OC    REPPGSTR,REPPGSTR   TEST FOR START PAGE USED                     
         BNZ   PS40                                                             
*                                                                               
         GOTO1 RNDPAGE,DMCB,1      READ FIRST PAGE OF REPORT                    
         BNE   EXIT                                                             
         CLI   TYPE,C'S'                                                        
         BNE   PS200                                                            
         B     PS100                                                            
*                                                                               
PS40     XC    DMCB(4),DMCB                                                     
         MVC   DMCB+1(3),REPPGSTR                                               
         GOTO1 RNDPAGE,DMCB                                                     
         BNE   EXIT                                                             
         B     PS200                                                            
*                                                                               
PS100    BAS   RE,BLDBUFF          BUILD BUFFER OF REQ DETAILS                  
*                                                                               
         LA    RE,REFBLOCK         CLEAR REFBLOCK                               
         LH    RF,=Y(L'REFBLOCK)                                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R2,REFBLOCK                                                      
         USING REFORMD,R2                                                       
         OI    REFSFLG1,REFSINIQ                                                
         MVC   REFID,SQREFID                                                    
         L     R1,AUTL                                                          
         MVC   REFAGY,TAGY-UTLD(R1)                                             
*                                                                               
         GOTO1 VREFORM,DMCB,REFBLOCK,REQBUFF,ACOMFACS                           
         TM    REFSFLG1,REFSERRQ                                                
         BNZ   ERROUT                                                           
         OI    REFSFLG1,REFSGETQ                                                
*                                                                               
         BAS   RE,REFOFRM          FILL FRAME WITH REPORT DATA OBJECTS          
*                                                                               
         GOTO1 WRTTWA,DMCB,ATMPBUF,3                                            
*                                                                               
         GOTO1 MARKREP,DMCB,0      MARK REPORT IS DOWNLOADING                   
         BNE   EXIT                                                             
*                                  PUT DETAIL INQUIRY OBJECT                    
         B     XIT                                                              
*                                                                               
PS200    BAS   RE,FILLFRM          FILL FRAME WITH REPORT DATA OBJECTS          
*                                                                               
         GOTO1 MARKREP,DMCB,0      MARK REPORT IS DOWNLOADING                   
         BNE   EXIT                                                             
*                                  PUT DETAIL INQUIRY OBJECT                    
         B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE PROCESSES MIDDLE MODE.  IT RETURNS THE NEXT FRAME FULL           
* OF REPORT DATA OBJECTS AND SETS THE LAST FRAME FLAG IF IT REACHES THE         
* END OF THE REPORT.                                                            
***********************************************************************         
PROCMID  NTR1                                                                   
         CLI   TYPE,C'S'                                                        
         BNE   MM200                                                            
*                                                                               
         GOTO1 READTWA,DMCB,ATMPBUF,3                                           
*                                                                               
         BAS   RE,REFOFRM          FILL FRAME WITH REPORT DATA OBJECTS          
*                                                                               
         GOTO1 WRTTWA,DMCB,ATMPBUF,3                                            
         B     MMX                                                              
*                                                                               
MM200    BAS   RE,FILLFRM          FILL FRAME WITH REPORT DATA OBJECTS          
*                                                                               
MMX      B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE PROCESSES THE END MODE.  IT SETS THE FLAG IN THE                 
* REPORT'S HEADER THAT SAYS IT HAS BEEN SENT.                                   
***********************************************************************         
PROCEND  NTR1                                                                   
         CLI   SENTFLAG,C'Y'       IF LAST TRANSACTION SENT REPORT              
         BNE   PE010                                                            
         GOTO1 MARKREP,DMCB,1      MARK REPORT IS SENT                          
*                                                                               
PE010    GOTO1 GETFACT,DMCB,(X'80',0),F#SESUN  UNPROTECT THE SESION             
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* MARK REPORT                                                                   
***********************************************************************         
PROCMARK NTR1                                                                   
         GOTO1 VALSPID             VALIDATE SPOOL ID                            
         BNE   PM10                                                             
*                                  VALIDATE REPORT                              
         GOTO1 GETNDX,DMCB,SIGNON2,REPSUBID,REPREPNO                            
         BE    PM20                                                             
         CLI   PQEOF,C'Y'          EOF - REPORT NOT FOUND                       
         BE    ERRRNF                                                           
         B     EXIT                DISK ERROR                                   
*                                                                               
PM10     OC    REPREPNO,REPREPNO   MUST BE OLD REPID                            
         BZ    ERRRID                                                           
*                                                                               
PM20     GOTO1 MARKREP,DMCB,1      MARK REPORT IS SENT                          
*                                                                               
PMX      B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* MARKS THE REPORT FOR DOWNLOADING                                              
*        P1 = 1 (MARK IT SENT)  P1=0 (MARK IT SENDING)                          
***********************************************************************         
MARKREP  NTR1                                                                   
         CLC   PCACTION,=Y(ACDISREP)  NO STATUS CHANGE FOR DISREP               
         BE    XIT                                                              
         CLC   PCACTION,=Y(ACDISALL)  NO STATUS CHANGE FOR DISREP               
         BE    XIT                                                              
*&&UK                                                                           
         CLC   NDX(2),=X'0E8B'     NO STATUS CHANGE FOR TARGET                  
         BE    XIT                                                              
         CLC   NDX(2),=X'1970'     NO STATUS CHANGE FOR BRANDX                  
         BE    XIT                                                              
*&&                                                                             
         USING PQRECD,R5           R5=PQ REPORT DSECT                           
         MVC   BYTE,3(R1)                                                       
         LA    RE,L'PQINDEX        SET INDEX LENGTH                             
         STH   RE,CINDXLN                                                       
         L     RF,ASYSFACS                                                      
         MVC   FIWENQ,VISGENQ-SYSFACD(RF)                                       
*                                                                               
         L     R5,ATIA             INIT PQ BUFFER                               
         GOTO1 DATAMGR,DMCB,(X'00',BUFFER),PRTQID,NDX,PQLINE,(R5),0             
         CLI   8(R1),0                                                          
         BNE   MRERROR             ERROR                                        
         MVC   CIDATA,12(R5)       SET CIDATA                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=CL8'SHMUSS'),(0,=CL8'ATTACH'),         +        
               (0,=CL8'PRTQ'),0,0,0                                             
         ICM   R1,15,DMCB+12                                                    
         JZ    *+2                 DEATH - CANNOT ATTACH                        
         ST    R1,FIWSHA                                                        
*                                                                               
         SAM31                                                                  
         GOTO1 PROTOFF                                                          
         MVC   FIWRES,PRTQID                                                    
         BRAS  RE,FIRSET                                                        
*                                                                               
         MVC   CIADDR,PSAVEB+SKFSTCI-SKBUFFD                                    
         MVC   FIWCIA,CIADDR                                                    
         BRAS  RE,FIRCN            CONVERT A(CI) TO A(NODE) AND REPORT#         
         BRAS  RE,FIRN1                                                         
         BNE   MRERROR             ERROR: NOT A VALID REPORT#                   
         BRAS  RE,FIRSLOCK         SHARED LOCK OF PRINT QUEUE                   
         BRAS  RE,FIRRLOCK         LOCK PRINT QUEUE REPORT                      
         GOTO1 PROTON                                                           
         SAM24                                                                  
*                                                                               
         CLI   BYTE,0              IS THIS A PRINTING REQUEST                   
         BE    MR020                                                            
*                                                                               
         TBIN  SECS                GET DATE TIME NOW                            
         SR    R0,R0                                                            
         D     R0,=F'60'           GET MINUTES                                  
         SR    R0,R0                                                            
         D     R0,=F'60'           GET HOURS                                    
         STC   R1,TIMEB                                                         
         STC   R0,TIMEB+1                                                       
         GOTO1 DATCON,DMCB,(5,0),(30,DATEC)                                     
         GOTO1 (RF),(R1),,(3,DATEB)                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),PRTQID,CIADDR,ATIA                   
         CLI   8(R1),0                                                          
         BNE   MRERRUR             ERROR: UNLOCK REPORT AND PRTQ                
*                                                                               
         MVC   PQDATED,DATEC       SET DATE/TIME PRINTED                        
         TM    PQTYP1,PQTYNCD      TEST NEW/OLD CMPRSD DATE                     
         BO    MR001                                                            
         GOTO1 DATCON,DMCB,(14,PQDATED),(2,PQDATED)                             
*                                                                               
MR001    MVC   PQAGEDD,DATEC       SET DATE/TIME PRINTED                        
         TM    PQTYP1,PQTYNCD      TEST NEW/OLD CMPRSD DATE                     
         BO    MR002                                                            
         GOTO1 DATCON,DMCB,(14,PQAGEDD),(2,PQAGEDD)                             
*                                                                               
MR002    MVC   PQTIMED,TIMEB                                                    
         L     RE,AUTL                                                          
         MVC   PQPRSYM,TSYM-UTLD(RE)    SET LOCATION FROM UTL                   
         IC    RE,PQPRCNT          BUMP NUMBER OF TIMES SENT                    
         LA    RE,1(RE)                                                         
         STC   RE,PQPRCNT                                                       
         NI    PQSTAT,255-(PQSTAC+PQSTTE)                                       
         OI    PQSTAT,PQSTSE       SET REPORT TO SENT STATUS                    
*                                                                               
         MVI   CMBYTE,0            CLEAR STATUS KEEP FLAG                       
*&&UK                                                                           
         CLC   PQDESC(3),=C'M14'   M14S SENT,KEEP                               
         BNE   *+12                                                             
         OI    PQSTAT,PQSTKE                                                    
         MVI   CMBYTE,C'K'         REMEMBER THIS FOR INDEX CHANGE TOO           
*&&                                                                             
         LA    R1,WORK             USE GETRET FOR RETAIN INFO                   
         XC    WORK,WORK                                                        
         USING GETRETD,R1                                                       
         MVC   GRDHRS,PQRETND                                                   
         MVC   GRDIDY(3),DATEB                                                  
         MVC   GRDITH(2),TIMEB                                                  
         GOTO1 GETRET                                                           
         LA    R1,WORK                                                          
         SR    RF,RF               CALCULATE BINARY 10MINS RET TIME             
         SR    RE,RE                                                            
         IC    RF,GRDOTH                                                        
         IC    RE,GRDOTM                                                        
         MH    RF,=H'60'           CALCULATE MINUTES                            
         AR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         STC   RF,DUB              SAVE IN DUB                                  
         MVC   FULL,GRDODY                                                      
         GOTO1 DATCON,DMCB,(3,FULL),(30,HALF)                                   
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      TEST NEW/OLD CMPRSD DATE                     
         BO    *+8                                                              
         MVI   DMCB+4,2            CONVERT TO OLD COMP                          
         BASR  RE,RF                                                            
         MVC   PQAGERD,HALF                                                     
         MVC   PQAGERT,DUB         SET RETAIN DATE/TIME                         
         DROP  R1                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'00',DMWRT),PRTQID,CIADDR,ATIA                    
         CLI   8(R1),0                                                          
         BNE   MRERRUR             ERROR: UNLOCK REPORT AND PRTQ                
*                                  READ INDEX PAGE                              
MR020    SAM31                                                                  
         GOTO1 PROTOFF                                                          
         L     R5,FIWNDA           POINT TO INDEX NODE IN MEMORY                
         LA    R5,SI1NDX-SI1PAR(R5) R5=A(INDEX)                                 
*                                                                               
         CLI   BYTE,1              IS THIS A SENT REQUEST                       
         BE    MR30                                                             
*                                                                               
         OI    PQSTAT-PQINDEX(R5),PQSTPG                                        
*                                                                               
         L     RF,ASYSFACS         RF= A(SYSFACS)                               
         L     RF,VSSB-SYSFACD(RF) RF= A(SSB)                                   
         L     RE,AUTL             RE=AUTL                                      
         MVC   PQAGEDD(2),TNUM-UTLD(RE)                                         
         OC    PQAGEDD(1),SSBSYSID-SSBD(RF)                                     
         MVI   PQAGERT,X'FE'       SET REPORT PRINTING                          
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         B     MR40                                                             
*                                                                               
MR30     NI    PQSTAT-PQINDEX(R5),255-(PQSTTE+PQSTAC)                           
         OI    PQSTAT-PQINDEX(R5),PQSTSE                                        
         CLI   CMBYTE,C'K'         IS THIS SENT,KEEP                            
         BNE   *+8                                                              
         OI    PQSTAT-PQINDEX(R5),PQSTKE                                        
         MVC   PQAGERD,HALF                                                     
         MVC   PQAGERT,DUB         SET NEW RETAIN INFO                          
*                                                                               
MR40     BRAS  RE,FIRRUNLK         UNLOCK PRINT QUEUE REPORT                    
         BRAS  RE,FIRSUNLK         UNLOCK SHARED PRINT QUEUE                    
         GOTO1 PROTON                                                           
         CR    RB,RB               SET CC=EQU                                   
         B     XIT                                                              
*                                                                               
MRERRUR  GOTO1 PROTOFF                                                          
         BRAS  RE,FIRRUNLK         UNLOCK PRINT QUEUE REPORT                    
         BRAS  RE,FIRSUNLK         UNLOCK SHARED PRINT QUEUE                    
         GOTO1 PROTON                                                           
MRERROR  B     ERRMRKRP                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE FILLS THE DATA FRAME WITH REPORT ITEMS.  IT GETS THE             
* ITEM INFO FROM PRSSTATE AND MOVES FROM ITEM TO ITEM BY MAKING CALLS           
* TO PRSNEXT.  PRSNEXT WILL TAKE CARE OF READING THE PRINT LINES FROM           
* THE PRINT QUEUE AND PARSING OUT THE NEXT ITEM TO PUT.  IF THE FRAME           
* REQUESTED IS THE FIRST FRAME OF THE REPORT, THEN PRSINIT WILL BE              
* CALLED TO PARSE OUT THE FIRST ITEM.                                           
***********************************************************************         
FILLFRM  NTR1                                                                   
*                                                                               
         CLI   OVERMODE,C'S'       IF OVERLAY MODE IS START                     
         BNE   FF10                                                             
         BAS   RE,PRSINIT          THEN PARSE OUT FIRST ITEM OF REPORT          
*                                                                               
* THIS LOOP WILL PUT ITEMS TO DATA FRAME UNTIL END OF FRAME REACHED OR          
* END OF REPORT REACHED                                                         
*                                                                               
FF10     CLI   EORFLAG,C'Y'        IF END OF REPORT THEN PUT EOD ITEM           
         BE    FF50                                                             
*                                                                               
         LA    R2,CCCHAR           PUT ITEM SPECIFIED IN PRSSTATE               
         A     R2,DDATA                                                         
         GOTO1 PUTITEM,DMCB,TPNUM,DLEN,(R2)                                     
         BNE   EXIT                                                             
*                                                                               
         CLI   EOFFLAG,C'Y'        IF END OF FRAME REACHED THEN DONE            
         BE    FFX                                                              
*                                                                               
         BAS   RE,PRSNEXT          ELSE PARSE OUT NEXT ITEM                     
         CLI   EOFFLAG,C'Y'        IF END OF FRAME REACHED THEN DONE            
         BE    FFX                                                              
         CLI   EORFLAG,C'Y'        IF END OF REPORT THEN DONE                   
         BE    FFX                                                              
         B     FF10                LOOP BACK                                    
*                                                                               
*                                  PUT END OF DATA ITEM                         
FF50     GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
*                                                                               
         GOTO1 MARKREP,DMCB,1      MARK REPORT IS SENT                          
*                                                                               
         CLI   EOFFLAG,C'Y'        IF END OF FRAME REACHED THEN DONE            
         BE    FFX                                                              
*                                                                               
         MVI   MDLAST,C'Y'         ELSE SET LAST FRAME FLAG TO 'Y'              
*                                                                               
         MVI   SENTFLAG,C'Y'       FLAG THAT WE SENT REPORT                     
*                                                                               
FFX      B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE INTIALIZES PRSSTATE TO FIRST ITEM IN REPORT.  IT DOES            
* SO BY SETTING DNEXTITM TO -1 AND CALLING PRSNEXT.  PRSNEXT WILL READ          
* THE FIRST LINE OF THE REPORT AND PARSE OUT THE FIRST ITEM.                    
***********************************************************************         
PRSINIT  NTR1                                                                   
*                                                                               
         MVI   EORFLAG,C'N'        SET END OF REPORT NOT YET REACHED            
*                                                                               
         MVC   DNEXTITM,=F'-1'     FORCE PRSNEXT TO READ FIRST LINE             
*                                                                               
         BAS   RE,PRSNEXT          PARSE FIRST ITEM                             
*                                                                               
PIX      B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE TAKES FROM PRSSTATE THE INFORMATION THAT IT NEEDS TO             
* PARSE OUT THE NEXT ITEM TO BE PUT TO THE FRAME.  IT UPDATES PRSSTATE          
* WITH THE ATTRIBUTES OF THE NEW ITEM.                                          
***********************************************************************         
PRSNEXT  NTR1                                                                   
*                                                                               
         CLC   DNEXTITM,=F'-1'     IF PRINT LINE HAS NO MORE ITEMS              
         BNE   PN100                                                            
*                                                                               
         BAS   RE,READLINE         THEN READ NEXT PRINT LINE                    
*                                                                               
         CLI   PQEOF,C'Y'          IF END OF FILE THEN SET EOR FLAG             
         BNE   PN5                     AND RETURN                               
         MVI   EORFLAG,C'Y'                                                     
         B     PNX                                                              
*                                                                               
PN5      CLI   CCCHAR,X'8B'        IF CC CHAR IS EJECT BEFORE PRINT             
         BNE   PN10                                                             
*                                                                               
         MVC   DNEXTITM,=F'0'      THEN SET D(NEXT ITEM) TO D(CC CHAR)          
         B     PN100                                                            
*                                                                               
PN10     MVC   DNEXTITM,=F'1'      ELSE SET D(NEXT ITEM) TO D(PDATA)            
*                                                                               
PN100    EQU   *                                                                
*                                                                               
         CLC   PCACTION,=Y(ACDISALL)                                            
         BNE   PN105               WE DON'T WANT ALL LINES                      
         CLC   DNEXTITM,=F'-2'                                                  
         BNE   *+14                                                             
         MVC   TPNUM,=A(ITNEWLIX)  THEN SET ITEM TO NEWLIX ITEM                 
         B     PN210                                                            
*                                                                               
PN105    CLC   DNEXTITM,=F'0'      IF D(NEXT ITEM) = D(CC CHAR)                 
         BNE   PN200                                                            
*                                                                               
         MVC   TPNUM,=A(ITFF)      THEN SET ITEM TO FORMFEED ITEM               
         MVC   DLEN,=F'0'                                                       
*                                                                               
         CLI   CCCHAR,X'8B'        IF CC CHAR IS EJECT BEFORE PRINT             
         BNE   PN110                                                            
*                                                                               
         MVC   DNEXTITM,=F'1'      THEN SET D(NEXT ITEM) TO D(PDATA)            
         B     PNX                                                              
*                                                                               
PN110    MVC   DNEXTITM,=F'-1'     ELSE NO MORE ITEMS THIS PRINT LINE           
         B     PNX                                                              
*                                                                               
PN200    CLC   DNEXTITM,PLEN       ELSE IF D(NEXT ITEM) =                       
         BNE   PN300                       D(END OF PRINT LINES BUFFER)         
*                                                                               
         MVC   TPNUM,=A(ITNEWLIN)  THEN SET ITEM TO NEWLINE ITEM                
         MVC   DLEN,=F'0'                                                       
*                                                                               
         CLC   PCACTION,=Y(ACDISALL)                                            
         BNE   PN204               WE DON'T WANT ALL LINES                      
*                                                                               
         MVI   SPACEING,1          SET SPACEING TO 1                            
         CLI   CCCHAR,X'09'        TEST CCCHAR IS WRITE & SPACE 1 LINE          
         BE    PN205                                                            
         CLI   CCCHAR,X'0B'        TEST CCCHAR IS WRITE & SPACE 1 LINE          
         BE    PN205                                                            
         MVI   SPACEING,2          SET SPACEING TO 2                            
         CLI   CCCHAR,X'11'        TEST CCCHAR IS WRITE & SPACE 2 LINE          
         BE    PN205                                                            
         CLI   CCCHAR,X'13'        TEST CCCHAR IS WRITE & SPACE 2 LINE          
         BE    PN205                                                            
         MVI   SPACEING,3          SET SPACEING TO 3                            
         CLI   CCCHAR,X'19'        TEST CCCHAR IS WRITE & SPACE 3 LINE          
         BE    PN205                                                            
         CLI   CCCHAR,X'1B'        TEST CCCHAR IS WRITE & SPACE 3 LINE          
         BE    PN205                                                            
*                                                                               
PN204    MVI   SPACEING,1          DEFAULT TO 1 LINE                            
*                                                                               
PN205    CLI   CCCHAR,X'89'        IF CC CHAR IS EJECT AFTER PRINT              
         BNE   PN210                                                            
*                                                                               
         MVC   DNEXTITM,=F'0'      THEN SET A(NEXT ITEM) TO A(CC CHAR)          
         B     PNX                                                              
*                                                                               
PN210    EQU   *                                                                
*                                                                               
         CLC   PCACTION,=Y(ACDISALL)                                            
         BNE   PN220               WE DON'T WANT ALL LINES                      
*                                                                               
         CLI   SPACEING,1                                                       
         BE    PN220                                                            
         SR    R1,R1               DEC SPACEING COUNT                           
         IC    R1,SPACEING                                                      
         BCTR  R1,0                                                             
         STC   R1,SPACEING                                                      
         MVC   DNEXTITM,=F'-2'                                                  
         B     PNX                                                              
*                                                                               
PN220    MVC   DNEXTITM,=F'-1'     ELSE NO MORE ITEMS THIS PRINT LINE           
         B     PNX                                                              
*                                                                               
PN300    BAS   RE,PRSTOKEN         ELSE PARSE TOKENIZED ITEM FROM PDATA         
*                                                                               
PNX      B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE PARSES OUT THE NEXT ITEM FROM THE PRINT LINE BY MAKING           
* CALLS TO LOWER LEVEL ROUTINES THAT HANDLE EITHER TEXT OR DATA FORMAT          
* REPORTS.                                                                      
***********************************************************************         
PRSTOKEN NTR1                                                                   
*                                                                               
         CLI   REPFORM,C'D'        IF REPORT IS IN DATA FORMAT                  
         BNE   PT10                                                             
         CLC   PQPAGE,=F'2'        AND ON SECOND PAGE                           
         BNE   PT10                                                             
*                                                                               
         BAS   RE,PRSDATA          THEN PARSE DATA FORMAT                       
         B     PTX                                                              
*                                                                               
PT10     BAS   RE,PRSTEXT          ELSE PARSE TEXT FORMAT                       
*                                                                               
PTX      B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE PARSES OUT THE NEXT ITEM FROM A PRINT LINE THAT CAME             
* FROM PAGE 2 OF A DATA FORMAT REPORT.  THESE TYPE OF LINES CONTAIN             
* STRINGS TERMINATED WITH QUOTES AND NUMBERS.  THE CORRESPONDING TYPES          
* ARE ITSTRING AND ITNUMBER.                                                    
***********************************************************************         
PRSDATA  NTR1                                                                   
*                                                                               
         L     R2,DNEXTITM         R2 = D(NEXT ITEM)                            
*                                                                               
         LA    RF,CCCHAR(R2)       IF ITEM STARTS WITH A QUOTE                  
         CLI   0(RF),X'7F'                                                      
         BE    PD05                                                             
         CLI   0(RF),X'4C'         IF ITEM STARTS WITH <                        
         BNE   PD50                                                             
*                                                                               
PD05     LR    RE,RF                                                            
         CLI   0(RE),X'4C'         IF ITEM STARTS WITH <                        
         BE    *+8                                                              
         LA    R2,1(R2)            THEN DONT BUMP TO NEXT CHAR                  
*                                                                               
         MVC   TPNUM,=A(ITSTRING)  SET ITEM TYPE TO ITSTRING                    
*                                                                               
         ST    R2,DDATA            AND SET D(DATA)                              
*                                                                               
*                                  LOOP TO FIND END OF STRING                   
*                                                                               
PD10     LA    RF,CCCHAR(R2)       WHILE END QUOTE NOT FOUND                    
*                                                                               
         CLI   0(RE),X'7F'         IF " START                                   
         BNE   *+16                                                             
         CLI   0(RF),X'7F'         LOOK FOR " END                               
         BE    PD20                                                             
         B     PD15                                                             
*                                                                               
         CLI   0(RF),X'6E'         ELSE LOOK FOR END >                          
         BE    PD20                                                             
*                                                                               
PD15     C     R2,PLEN             IF END OF LINE REACHED THEN ERROR            
         BE    ERRSNT                                                           
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT CHAR                            
         B     PD10                AND LOOP BACK                                
*                                                                               
PD20     LR    RF,R2               SET DATA LENGTH = D(END OF STRING)           
         S     RF,DDATA                - D(DATA)                                
         ST    RF,DLEN                                                          
*                                                                               
         LA    R2,1(R2)            BUMP PAST END QUOTE                          
         CLI   0(RE),X'4C'         IF ITEM STARTS WITH <                        
         BNE   PD100                                                            
*                                                                               
         LA    RF,1(RF)            BUMP LEN BY 1 MORE                           
         ST    RF,DLEN                                                          
         LA    R2,1(R2)            BUMP PAST ,                                  
         GOTO1 PUTITEM,DMCB,ITNEWLIN,0      SEND NEW LINE                       
         BNE   EXIT                                                             
         B     PD100                                                            
*                                                                               
PD50     MVC   TPNUM,=A(ITNUMBER)  ELSE SET ITEM TYPE TO ITNUMBER               
         ST    R2,DDATA            AND SET D(DATA)                              
*                                                                               
*                                  LOOP TO FIND END OF NUMBER                   
*                                                                               
PD60     C     R2,PLEN             WHILE NOT END OF LINE                        
         BE    PD70                                                             
         LA    RF,CCCHAR(R2)       AND NOT SPACE CHAR                           
         CLI   0(RF),C' '                                                       
         BE    PD70                                                             
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT CHAR                            
         B     PD60                AND LOOP BACK                                
*                                                                               
PD70     LR    RF,R2               SET DATA LENGTH = D(END OF NUMBER)           
         S     RF,DDATA                - D(DATA)                                
         ST    RF,DLEN                                                          
*                                                                               
*                                  LOOP TO FIND NEXT ITEM                       
*                                                                               
PD100    C     R2,PLEN             WHILE NOT END OF LINE                        
         BNL   PD150                                                            
         LA    RF,CCCHAR(R2)       AND CHAR IS A SPACE                          
         CLI   0(RF),C' '                                                       
         BH    PD150                                                            
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT CHAR                            
         B     PD100               AND LOOP BACK                                
*                                                                               
PD150    ST    R2,DNEXTITM         SET A(NEXT ITEM)                             
*                                                                               
PDX      B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE PARSES OUT THE NEXT ITEM FROM A PRINT LINE THAT IS IN            
* THE TEXT REPORT FORMAT.  THESE TYPE OF LINES CONTAIN CHARACTER                
* DATA MIXED IN WITH SPECIAL CHARACTERS.  THE ROUTINE WILL SEPARATE             
* THE CHARACTER DATA INTO STRING ITEMS AND THE SPECIAL CHARACTERS INTO          
* SPECIAL CHARACTERS ITEMS.                                                     
***********************************************************************         
PRSTEXT  NTR1                                                                   
*                                                                               
         L     R2,DNEXTITM         R2 = D(NEXT ITEM)                            
         ST    R2,DDATA            SET D(DATA)                                  
*                                                                               
         LA    RF,CCCHAR(R2)       IF FIRST CHAR IS SPECIAL BOX CHAR            
         CLI   0(RF),X'40'                                                      
         BNL   PTX50                                                            
*                                                                               
PTX10    MVC   TPNUM,=A(ITSPECHS)  THEN SET ITEM TYPE TO SPECIAL CHARS          
*                                                                               
*                                  LOOP TO FIND END OF SPECIAL CHARS            
*                                                                               
         C     R2,PLEN             WHILE NOT END OF LINE                        
         BE    PTX100                                                           
         LA    RF,CCCHAR(R2)       AND CHAR IS SPECIAL CHAR                     
         CLI   0(RF),X'40'                                                      
         BNL   PTX100                                                           
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT CHAR                            
         B     PTX10               AND LOOP BACK                                
*                                                                               
PTX50    MVC   TPNUM,=A(ITSTRING)  ELSE SET ITEM TYPE TO STRING                 
*                                                                               
*                                  LOOP TO FIND END OF STRING                   
*                                                                               
PTX60    C     R2,PLEN             WHILE NOT END OF LINE                        
         BE    PTX100                                                           
         LA    RF,CCCHAR(R2)       AND CHAR IS NOT SPEICAL CHAR                 
         CLI   0(RF),X'40'                                                      
         BL    PTX100                                                           
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT CHAR                            
         B     PTX60               AND LOOP BACK                                
*                                                                               
PTX100   ST    R2,DNEXTITM         SET A(NEXT ITEM)                             
*                                                                               
         LR    RF,R2               SET DATA LENGTH = D(END OF DATA)             
         S     RF,DDATA                - D(DATA)                                
         ST    RF,DLEN                                                          
*                                                                               
         CLC   TPNUM,=A(ITSPECHS)  IF ITEM TYPE IS ITSPECHS                     
         BNE   PTXX                                                             
*                                                                               
         L     RE,DDATA            THEN TRANSLATE ITEM DATA TO SPECIAL          
         LA    RE,CCCHAR(RE)           VALUES FOR ALINK                         
         L     RF,DLEN                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         TR    0(0,RE),SPECCHRS                                                 
*                                                                               
PTXX     B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE READS THE NEXT LINE OF THE REPORT AND PUTS IT IN THE             
* PRSSTATE BUFFER.  IF THE REPORT IS IN DATA FORMAT AND THE PAGE                
* NUMBER IS 2, THEN IT WILL CONTINUE TO READ LINES AND BUFFER THEM              
* UNTIL A LINE IS READ THAT HAS A SEMICOLON AT THE END OF IT.  THE              
* ROUTINE RETURNS THE LENGTH OF THE BUFFER IN PLEN.                             
***********************************************************************         
READLINE NTR1                                                                   
         CLI   FLREAD,C'N'         IF FIRST LINE OF TRANS NOT YET READ          
         BNE   RL10                                                             
*                                                                               
         GOTO1 FIRSTLIN            THEN CALL FIRSTLIN                           
         BNE   EXIT                                                             
         MVI   FLREAD,C'Y'         SET FIRST LINE OF TRANSACTION READ           
         B     RL20                                                             
*                                                                               
RL10     GOTO1 NEXTLIN             ELSE READ NEXT LINE                          
         BNE   EXIT                                                             
*                                                                               
RL20     CLI   PQEOF,C'Y'          IF END OF REPORT REACHED THEN RETURN         
         BE    RLX                                                              
*                                                                               
         OC    REPPGEND,REPPGEND   TEST FOR PAGE LIMIT SET                      
         BZ    RL21                                                             
         CLC   PQPAGE,REPPGEND                                                  
         BNH   RL21                                                             
         MVI   PQEOF,C'Y'          FORCE EOF IF END PAGE READ                   
         B     RLX                                                              
*                                                                               
*                                  ELSE COPY DATA TO PRSSTATE BUFFER            
RL21     MVC   CCCHAR(L'PQPLIN),PQPLIN                                          
*                                                                               
         L     RF,PQLEN            R3 = A(END OF FIRST PRINT LINE)              
         LA    R3,CCCHAR+1(RF)                                                  
*                                                                               
         CLI   REPFORM,C'D'        IF REPORT IS IN DATA FORMAT                  
         BNE   RL100                                                            
         CLC   PQPAGE,=F'2'        AND PAGE NUMBER IS 2                         
         BNE   RL100                                                            
*                                                                               
* THEN CONTINUE TO BUFFER PRINT LINES UNTIL A PRINT LINE IS FOUND WITH          
* A SEMICOLON AT THE END                                                        
*                                                                               
         BCTR  R3,0                R3 = A(LAST CHAR OF FIRST PRNTLINE)          
         LA    R4,CCCHAR           R4 = A(END OF BUFFER)                        
         AH    R4,=Y(BUFLEN)                                                    
*                                                                               
RL30     CLI   0(R3),X'5E'         WHILE LAST CHAR <> SEMICOLON                 
         BE    RL50                                                             
*                                                                               
         LA    R3,1(R3)            BUMP R3 PAST LAST CHAR                       
*                                                                               
         OC    PQLEN,PQLEN         GET NEXT LINE IF THIS ONE EMPTY              
         BZ    RL40                                                             
*                                                                               
         MVI   0(R3),C' '          PUT A SPACE THERE                            
         LA    R3,1(R3)            BUMP PAST                                    
*                                                                               
RL40     GOTO1 NEXTLIN             READ NEXT LINE FROM PRINT QUEUE              
         BNE   EXIT                                                             
*                                                                               
         OC    PQLEN,PQLEN         GET NEXT LINE IF THIS ONE EMPTY              
         BZ    RL40                                                             
*                                                                               
         CLI   PQEOF,C'Y'          IF END OF FILE THEN ERROR                    
         BE    ERRBFEOR                                                         
*                                                                               
         LA    RF,1(R3)            IF NEW LINE WILL OVERFLOW BUFFER             
         A     RF,PQLEN                                                         
         CR    RF,R4                                                            
         BH    ERRBFOV             THEN GIVE ERROR CONDITION                    
*                                                                               
         L     RF,PQLEN            COPY NEW LINE IN                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),PQPLIN+1                                                 
         LA    R3,0(R3,RF)         BUMP TO LAST CHAR                            
*                                                                               
         B     RL30                LOOP BACK                                    
*                                                                               
RL50     MVI   0(R3),C' '          CLEAR OUT SEMICOLON                          
*                                                                               
RL100    LR    RF,R3               CALCULATE FINAL PRINT LINE BUF LEN           
         LA    RE,CCCHAR                                                        
         SR    RF,RE                                                            
         ST    RF,PLEN                                                          
*                                                                               
RLX      B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* FILL FRAME WITH MAD OBJECTS FROM REFORM                                       
***********************************************************************         
REFOFRM  NTR1                                                                   
*                                                                               
         LA    R2,REFBLOCK         R2=REFBLOCK                                  
         USING REFORMD,R2                                                       
*                                                                               
         CLI   NEWLSENT,0          IF NEWLSENT=0 THEN INIT                      
         BE    RF01                                                             
*                                                                               
         CLI   NEWLSENT,C'N'       IF NEWL WAS SENT CONTINUE                    
         BNE   RF22                                                             
*                                                                               
         GOTO1 PUTITEM,DMCB,ITNEWLIN,0      SEND NEW LINE                       
         BNE   EXIT                                                             
         TM    REFSFLG1,REFSGETQ   TEST FOR NEXT REC REQUIRED                   
         BZ    RF21                                                             
*                                                                               
RF01     MVI   REFSFLG1,REFSDATQ   SET DATA LINE INPUT FLAG                     
*                                                                               
RF10     CLI   FLREAD,C'Y'         TEST PQ READS INITIALISED                    
         BE    RF11                                                             
         GOTO1 FIRSTLIN            FIRST LINE                                   
         MVI   FLREAD,C'Y'                                                      
*                                                                               
RF11     CLI   PQEOF,C'Y'          IF END OF REPORT THEN PUT EOD ITEM           
         BE    RF50                                                             
*                                                                               
         MVI   REFSFLG1,REFSDATQ   SET DATA LINE INPUT FLAG                     
         XC    REFDATA,REFDATA                                                  
         ICM   R1,15,PQLEN         EX LEN IS PQLEN -1                           
         SH    R1,=H'1'                                                         
         BM    RF20                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   REFDATA(0),PQPLIN+1                                              
RF20     MVC   REFDLEN,PQLEN       DATA LEN IS PQLEN                            
*                                                                               
RF21     GOTO1 VREFORM,DMCB,REFBLOCK,AREFAREA,ACOMFACS                          
         TM    REFSFLG1,REFSERRQ                                                
         BZ    *+6                                                              
         DC    H'0'                ERROR RETURN FROM REFORM                     
*                                                                               
RF22     TM    REFSFLG1,REFSOUTQ   ANY OUTPUT DATA TO PROCESS                   
         BZ    RF30                                                             
*                                                                               
         L     R3,AREFAREA         PUT TO FRAME                                 
         SR    RF,RF                                                            
         ICM   RF,3,0(R3)                                                       
         SH    RF,=H'4'                                                         
         LA    R3,4(R3)                                                         
*                                                                               
         GOTO1 PUTITEM,DMCB,ITSTRING,(RF),(R3)                                  
         BNE   EXIT                                                             
*                                                                               
         CLI   EOFFLAG,C'Y'        IF END OF FRAME REACHED THEN DONE            
         BE    RFX                                                              
*                                                                               
         MVI   NEWLSENT,C'Y'                                                    
         GOTO1 PUTITEM,DMCB,ITNEWLIN,0                                          
         BNE   EXIT                                                             
*                                                                               
         CLI   EOFFLAG,C'Y'        IF END OF FRAME REACHED THEN DONE            
         BNE   RF23                                                             
         MVI   NEWLSENT,C'N'                                                    
         B     RFX                                                              
*                                                                               
RF23     TM    REFSFLG1,REFSGETQ   DO WE WANT TO GET NEXT LINE                  
         BZ    RF21                                                             
*                                                                               
RF30     CLI   FLREAD,C'Y'         TEST PQ INITIALISED                          
         BNE   RF10                                                             
         GOTO1 NEXTLIN             GET NEXT PQ LINE                             
         CLI   PQEOF,C'Y'          IF END OF REPORT THEN DONE                   
         BNE   RF10                LOOP BACK                                    
RF31     MVI   NEWLSENT,0          SET INIT FOR END OF REPORT                   
         B     RFX                                                              
*                                                                               
*                                  PUT END OF DATA ITEM                         
RF50     CLI   NEWLSENT,0          ONLY IF FIRST OR LAST FRAME                  
         BNE   RF31                                                             
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
*                                                                               
         GOTO1 MARKREP,DMCB,1      MARK REPORT IS SENT                          
*                                                                               
         CLI   EOFFLAG,C'Y'        IF END OF FRAME REACHED THEN DONE            
         BE    RFX                                                              
*                                                                               
         MVI   MDLAST,C'Y'         ELSE SET LAST FRAME FLAG TO 'Y'              
*                                                                               
RFX      B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* BUILD BUFFER OF REQUEST DETAILS                                               
***********************************************************************         
BLDBUFF  NTR1                                                                   
*                                                                               
         LA    RE,REQBUFF          CLEAR REQBUFF                                
         LH    RF,=Y(L'REQBUFF)                                                 
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,8,=C' '                                                       
         MVCL  RE,R0                                                            
*                                                                               
         LA    R2,REQBUFF          R2=A(BUFFER)                                 
*                                                                               
         GOTO1 FIRSTLIN            FIRST LINE                                   
         MVI   FLREAD,C'Y'                                                      
*                                                                               
BF10     CLI   PQEOF,C'Y'          IF END OF REPORT THEN EXIT                   
         BE    BFX                                                              
*                                                                               
         ICM   R1,15,PQLEN         COPY LINE                                    
         SH    R1,=H'1'                                                         
         BM    BF21                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),PQPLIN+1                                                 
*                                                                               
BF20     LA    R2,1(R1,R2)         SET ZERO DELIMITER                           
         MVI   0(R2),0                                                          
         LA    R2,1(R2)                                                         
*                                                                               
BF21     GOTO1 NEXTLIN             ELSE PARSE OUT NEXT ITEM                     
         CLC   PQPAGE,=F'1'                                                     
         BE    BF10                LOOP BACK                                    
*                                                                               
         MVI   0(R2),0             SET DOUBLE ZERO END                          
*                                                                               
BFX      B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* RETURN ERROR OBJECT                                                           
***********************************************************************         
         USING REFORMD,R2                                                       
ERROUT   MVC   APPLERR,REFSCODE+1                                               
         GOTO1 HEXOUT,DMCB,APPLERR,FULL,2                                       
         GOTO1 PUTITEM,DMCB,A(ITGENERR),4,FULL                                  
         BNE   EXIT                                                             
         MVI   MDLAST,C'Y'                                                      
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ERROR EXITS                                                                   
***********************************************************************         
ERRRID   MVC   MDACTION,=Y(DLIDERR)  REPORT ID ERROR                            
         J     EXIT                                                             
ERRRNF   MVC   MDACTION,=Y(DLRNFERR) REPORT NOT FOUND                           
         J     EXIT                                                             
ERRJCL   MVC   MDACTION,=Y(DLJCLREP) REPORT HAS JCL IN IT (NOT READY)           
         J     EXIT                                                             
ERRERROR MVC   MDACTION,=Y(DLREPERR) REPORT IS IN ERROR                         
         J     EXIT                                                             
ERRSNT   MVC   MDACTION,=Y(DLSTRTRM) STRING NOT TERMINATED BY QUOTE             
         J     EXIT                                                             
ERRBFEOR MVC   MDACTION,=Y(DLBFEOR)  END OF REPORT WHILE BUFFERING              
         J     EXIT                                                             
ERRBFOV  MVC   MDACTION,=Y(DLBFOV)   BUFFER OVERFLOW                            
         J     EXIT                                                             
ERRMRKRP MVC   MDACTION,=Y(DLMRKREP) DISK ERROR DURING MARKING REPORT           
         J     EXIT                                                             
*                                                                               
EXIT     L     RD,SAVEDRD            RESTORE RD                                 
XIT      XIT1                                                                   
         EJECT                                                                  
                                                                                
***********************************************************************         
* OVERLAY CONSTANTS, ETC.                                                       
***********************************************************************         
BUFFER   DC    CL8'BUFFER'                                                      
GFILE    DC    CL8'GFILE'                                                       
INDEX    DC    CL8'INDEX'                                                       
RANDOM   DC    CL8'RANDOM'                                                      
READL    DC    CL8'READ'                                                        
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
PRTQUE   DC    CL8'PRTQUE'                                                      
*                                                                               
* TRANSLATION TABLE FOR BOX CHARS TO DATA OBJECT                                
*                                                                               
SPECCHRS DC    C'ABCDEFGHIJKLMNOP'                                              
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* SHARED MEMORY ROUTINES                                                        
***********************************************************************         
       ++INCLUDE SHFIR                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* OVERLAY WORKING STORAGE                                                       
***********************************************************************         
OVERD    DSECT                                                                  
*                                                                               
* MISCELLANEOUS VARIABLES                                                       
*                                                                               
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
VREFORM  DS    A                                                                
AREFAREA DS    A                                                                
SVRE     DS    A                                                                
SVR4     DS    A                                                                
NUMWORK  DS    CL10                WORK AREA FOR NUMBER OF PAGES/LINES          
SENTFLAG DS    C                   DONE SENDING REPORT (Y/N)                    
*                                                                               
* VARIABLES SET IN INIT                                                         
*                                                                               
TYPE     DS    C                                                                
SQREFID  DS    CL5                                                              
FLREAD   DS    C                   FLAGS FIRST PRINT LINE HAS BEEN READ         
NEWLSENT DS    C                                                                
*                                                                               
* VARIABLES USED BY MARKREP                                                     
*                                                                               
TIMEB    DS    XL2                                                              
DATEC    DS    XL2                 CMPRSD DATE NEW FORMAT                       
DATEB    DS    XL3                                                              
         DS    0H                                                               
*                                                                               
* SHARED MEMORY DATA                                                            
*                                                                               
       ++INCLUDE SHFIW                                                          
*                                                                               
* CIDATA INFO, THIS IS THE PRE-DTF TABLE INFO                                   
*                                                                               
       ++INCLUDE DMPRTQW                                                        
*                                                                               
* PARSE STATE - VARIABLES SET BY PRS(INIT, NEXT) AND READLINE                   
*                                                                               
SPACEING DS    X                   COUNTER USED FOR SPACEING                    
EORFLAG  DS    C                   END OF REPORT FLAG (Y/N)                     
TPNUM    DS    A                   ITEM TYPE NUMBER                             
DLEN     DS    A                   ITEM DATA LENGTH                             
DDATA    DS    A                   DISP TO ITEM DATA                            
DNEXTITM DS    A                   DISP TO NEXT ITEM                            
PLEN     DS    F                   PRINT LINES BUFFER LENGTH                    
CCCHAR   DS    CL(BUFLEN)          PRINT LINES BUFFER INCLUDING CCCHAR          
*                                      OF FIRST PRINT LINE                      
BUFLEN   EQU   3200                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* OTHER DSECTS                                                                  
***********************************************************************         
TEMPD    DSECT                                                                  
TEMPAREA DS    0CL18432                                                         
REQBUFF  DS    CL(40*80)                                                        
REFBLOCK DS    CL(REFORML)                                                      
REFAREA  DS    4096C                                                            
         EJECT                                                                  
                                                                                
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTMADEQUS                                                      
         EJECT                                                                  
       ++INCLUDE CTMADDSECT                                                     
         EJECT                                                                  
       ++INCLUDE DDREFORMD                                                      
         EJECT                                                                  
       ++INCLUDE DMPRTQD                                                        
         EJECT                                                                  
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DMPRTQS                                                        
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE DDGETRETD                                                      
         EJECT                                                                  
       ++INCLUDE SHFID                                                          
         EJECT                                                                  
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
* DDCOMFACS                                                                     
* CTGENFILE                                                                     
* IHAASCB                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTGENFILE                                                      
         IHAASCB LIST=YES                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015CTMAD01   07/20/20'                                      
         END                                                                    
