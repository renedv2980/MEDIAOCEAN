*          DATA SET CTSFM54    AT LEVEL 002 AS OF 10/21/15                      
*PHASE TA0A54A                                                                  
***********************************************************************         
*  TITLE:        CTSFM54 -- STAFF RECORDS MAINTENANCE/LIST/REPORT     *         
*  COMMENTS:     MAINTAINS STAFF RECORD ON GENDIR/GENFIL              *         
*  CALLED FROM:  SFM CONTROLLER (TA0A00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*  INPUTS:       SCREENS CTSFMA4 (MAINTENANCE)                        *         
*                        CTSFMB4 (LIST)                               *         
*                        CTSFMC4 (REPORT)                             *         
*  OUTPUTS:      UPDATED STAFF  RECORDS, LIST, OR REPORT.             *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
***********************************************************************         
         TITLE 'TA0A54 - SCROLLER DATATYPE RECORD MAINT/LIST/REPORT'            
TA0A54   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0A54**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING TA0AFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         EJECT                                                                  
***********************************************************************         
* THIS CODE MUST BE EXECUTED BEFORE THE PREP CALL                     *         
* THE DEFAULT 3-CHR ID XXX IS THE NAME INPUT IN NOW,XXX OR SOON,XXX   *         
* THE DEFAULT CLASS IS Z                                              *         
* THE DESCRIPTION IS TAKEN FROM THE REPORT PROFILE                    *         
***********************************************************************         
         MVC   REMUSER,=C'STF'     SET 3-CHR ID OF REPORT                       
         OI    GENSTAT3,NOCLRSPK   DONT CLEAR SPOOL KEY                         
         OI    SPOOLIND,SPUINIT    SET USER CAN INITIALIZE FIELDS               
         LA    RF,SPOOLKEY                                                      
         USING PQPLD,RF                                                         
         XC    SPOOLKEY,SPOOLKEY                                                
         MVI   PLCLASS,C'A'        SET REPORT CLASS                             
         MVC   PLDESC,=CL11'STAFF LIST'                                         
         DROP  RF                                                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    PREP                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                        *         
***********************************************************************         
VK       XC    STAFID,STAFID                                                    
         CLI   ACTEQU,ACTREP       REPORT ACTION?                               
         BE    VKKEY20             IF SO, JUST BUILD THE KEY                    
*                                                                               
         CLI   ACTEQU,ACTLIST      LIST ACTION?                                 
         BNE   VKMNT10             NO, THEN MAINT ACTION, BRANCH                
*                                                                               
         CLI   MSLSTAFH+5,0                                                     
         BE    VKKEY20             NO INPUT=ALL                                 
         ZIC   R3,MSLSTAFH+5       R3=LENGTH OF STAFID INPUT                    
         BCTR  R3,0                DECR FOR EX                                  
         EXMVC R3,STAFID,MSLSTAF   PUT STAFID IN R3.STAFID=START @              
         OC    STAFID,SPACES       BLANK PAD ID                                 
         CLC   STAFID,=C'ALL '     WAS 'ALL' REQUESTED?                         
         BNE   VKKEY20             IF NOT, GO STRAIGHT TO KEY BUILD             
         XC    STAFID,STAFID       ELSE CLEAR STAFID                            
         B     VKKEY20             BUILD KEY                                    
*                                                                               
VKMNT10  LA    R2,MSMSTAFH         STAFF FIELD REQUIRED                         
         GOTO1 ANY                                                              
         ZIC   R3,MSMSTAFH+5       R3=LENGTH OF STAFID INPUT                    
         BCTR  R3,0                DECR FOR EX                                  
         EXMVC R3,STAFID,MSMSTAF   SAVE STAFID                                  
         OC    STAFID,SPACES       BLANK PAD ID                                 
         B     VKKEY20                                                          
*                                                                               
VKKEY20  LA    R4,KEY              BUILD KEY                                    
         USING MSTFKEYD,R4                                                      
         XC    KEY,KEY                                                          
         MVI   MSTFSYS,MSTFSYSQ    RECORD SYSTEM CODE                           
         MVI   MSTFTYP,MSTFTYPQ    RECORD TYPE CODE                             
         MVC   MSTFID,STAFID                                                    
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                     *         
***********************************************************************         
VR       L     R4,AIO              BUILD RECORD                                 
         USING MSTFKEYD,R4                                                      
         MVC   STAFID,MSTFID       SAVE STAF ID FOR LATER USE                   
*                                                                               
         MVI   ELCODE,MSNAMELQ     NAME ELEMENT                                 
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING MSNAMD,R6                                                        
         XC    ELEM,ELEM                                                        
         MVI   MSNAMEL,MSNAMELQ                                                 
         LA    R2,MSMNAMEH         STAFF MEMBER'S FULL NAME                     
         GOTO1 ANY                                                              
         ZIC   R3,MSMNAMEH+5       LENGTH OF NAME IN R3                         
         BCTR  R3,0                DECR FOR EX                                  
         EXMVC R3,MSNAME,WORK      STORE VAR LENGTH NAME                        
         LA    R3,MSNAMOVQ+1(R3)   TOTAL LENGTH OF ELEMENT                      
         STC   R3,MSNAMLN          STORE LENGTH IN ELEMENT                      
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,MSNUMELQ     TELEPHONE NUMBERS ELEMENT                    
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING MSNUMD,R6                                                        
         XC    ELEM,ELEM                                                        
         MVI   MSNUMEL,MSNUMELQ                                                 
         MVI   MSNUMLN,MSNUMLNQ                                                 
*                                                                               
         LA    R2,MSMEXTNH         OFFICE EXTENSION                             
         GOTO1 ANY                                                              
         MVC   MSNUMOF,WORK                                                     
*                                                                               
         LA    R2,MSMHOMEH         HOME TELEPHONE NUMBER                        
         MVC   MSNUMHM,SPACES      SET TO BLANKS IN CASE NO INPUT               
         CLI   MSMHOMEH+5,0        ANY INPUT IN THIS FIELD?                     
         BE    VR15                                                             
         CLI   MSMHOMEH+5,12       12 DIGITS?                                   
         BE    *+14                YES                                          
         MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
         MVC   MSNUMHM,MSMHOME                                                  
VR15     GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
VR20     B     DR                  DISPLAY THE RECORD                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                      *         
***********************************************************************         
DR       LA    R0,MSMTAGH          LAST FIELD ON SCREEN                         
         LA    R2,MSMNAMEH         FIRST FIELD HEADER (AFTER KEY)               
*                                                                               
DR10     ZIC   R1,0(R2)            LENGTH OF FIELD + HEADER                     
         SH    R1,=H'17'           MINUS HEADER, EXTEN, AND 1 FOR EX            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES      BLANK OUT FIELD                              
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
DR20     ZIC   R1,0(R2)            RESTORE LENGTH                               
         AR    R2,R1               NEXT SCREEN FIELD                            
         CR    R2,R0               END OF SCREEN?                               
         BE    *+16                                                             
         TM    1(R2),X'20'         IS FIELD IS PROTECTED?                       
         BZ    DR10                NO-CLEAR IT                                  
         B     DR20                YES-BUMP TO NEXT FIELD                       
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,MSNAMELQ     NAME ELEMENT DSECT                           
         BAS   RE,GETEL            FIND ELEMENT                                 
         BE    *+6                 IF THERE, CONTINUE, ELSE BUG                 
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING MSNAMD,R6           NAME ELEMENT'S DSECT                         
         ZIC   R3,MSNAMLN          GET LENGTH OF ELEMENT                        
         LA    R4,MSNAMOVQ+1       MINUS OVERHEAD MINUS 1 FOR EX                
         SR    R3,R4               GET LENGTH OF NAME FIELD                     
         EXMVC R3,MSMNAME,MSNAME   STORE VAR LENGTH NAME                        
         OI    MSMNAMEH+6,X'80'    TRANSMIT                                     
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,MSNUMELQ     TELEPHONE INFORMATION                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING MSNUMD,R6           NUMBER ELEMENT'S DSECT                       
         MVC   MSMEXTN,MSNUMOF     LOAD IN EXTENTION                            
         OI    MSMEXTNH+6,X'80'    TRANSMIT                                     
         MVC   MSMHOME,MSNUMHM     LOAD IN HOME TEL NUMBER                      
         OI    MSMHOMEH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                         *         
***********************************************************************         
DK       L     R4,AIO              SELECTED RECORD                              
         USING MSTFKEYD,R4                                                      
         MVC   MSMSTAF,MSTFID      STAFF ID                                     
         OI    MSMSTAFH+6,X'80'                                                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ON-SCREEN LIST                                                      *         
***********************************************************************         
LR       LA    R4,KEY                                                           
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR10                                                             
*                                                                               
         MVI   MSTFSYS,MSTFSYSQ    SYSTEM                                       
         MVI   MSTFTYP,MSTFTYPQ    RECORD TYPE                                  
         MVC   MSTFID,STAFID       STAFF ID                                     
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR20     LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR30     CLC   KEY(2),SAVEKEY      SAME SYSTEM/PROGRAM?                         
         BNE   LRX                 NO MORE DATATYPES TO LIST                    
*                                                                               
         GOTO1 GETREC              GET THE SCROLLER DATATYPE RECORD             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
*                                                                               
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         MVC   LSTSTAFF,MSTFID                                                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,MSNAMELQ     NAME ELEMENT DSECT                           
         BAS   RE,GETEL            FIND ELEMENT                                 
         BE    *+6                 IF THERE, CONTINUE, ELSE BUG                 
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING MSNAMD,R6           NAME ELEMENT'S DSECT                         
         ZIC   R3,MSNAMLN          GET LENGTH OF NAME                           
         LA    R2,MSNAMOVQ+1       OVERHEAD TO NAME FIELD IN ELEM               
         SR    R3,R2                                                            
         EXMVC R3,LSTNAME,MSNAME   STORE VAR LENGTH NAME                        
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,MSNUMELQ     TELEPHONE INFORMATION                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING MSNUMD,R6           NUMBER ELEMENT'S DSECT                       
         MVC   LSTEXTN,MSNUMOF     LOAD IN EXTENTION                            
         MVC   LSTHOME,MSNUMHM     LOAD IN HOME TEL NUMBER                      
*                                                                               
         GOTO1 LISTMON             SEND RECORD TO SCREEN                        
         B     LR20                NEXT RECORD                                  
*                                                                               
LRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* OFFLINE PRINT                                                       *         
***********************************************************************         
PREP     EQU   *                                                                
         SR    R5,R5               R5=ZERO FOR FIRST TIME                       
*                                                                               
PR       LA    R1,HEDSPECS         HEADLINE SPECS                               
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK                                                        
         ST    R1,HEADHOOK                                                      
*                                                                               
PR10     LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   MSTFSYS,MSTFSYSQ    SYSTEM                                       
         MVI   MSTFTYP,MSTFTYPQ    RECORD TYPE                                  
         MVC   MSTFID,STAFID       SYSTEM/PROGRAM                               
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    PR30                                                             
         DC    H'0'                                                             
*                                                                               
PR20     MVC   KEYSAVE,KEY                                                      
         LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    PR30                                                             
         DC    H'0'                                                             
*                                                                               
PR30     CLC   KEY(2),SAVEKEY      TEST NO MORE RECORDS                         
         BNE   PR50                                                             
*                                                                               
         GOTO1 GETREC              GET THE SCROLLER DATATYPE RECORD             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
         MVC   PRTSTAF,MSTFID      PLACE STAFID ON PRINT LINE                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,MSNAMELQ     DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING MSNAMD,R6                                                        
         ZIC   R3,MSNAMLN          LENGTH OF ELEMENT                            
         LA    R4,MSNAMOVQ+1       LENGTH OF OVERHEAD-1 FOR EX                  
         SR    R3,R4               TAKE OUT OVERHEAD FROM LENGTH                
         EXMVC R3,PRTNAME,MSNAME   PLACE NAME ON PRINT LINEINE                  
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,MSNUMELQ     TELEPHONE NUMBERS ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING MSNUMD,R6                                                        
         MVC   PRTEXTN,MSNUMOF     OFFICE NUMBER                                
         MVC   PRTHOME,MSNUMHM     HOME TEL NUMBER                              
         DROP  R6                                                               
*                                                                               
PR40     GOTO1 SPOOL,DMCB,(R8)     PRINT RECORD                                 
                                                                                
***********************************************************************         
* THIS CODE ATTEMPTS TO CHANGE SOME FIELDS DIRECT IN THE PQ BUFFER    *         
* IT IS DONE AFTER THE FIRST ADD OF A PRINT LINE                      *         
* IT ONLY WORKS FOR NOW REPORTS (IF NOP TEST FOR SOON)                *         
* CANT CHANGE ANY PQKEY FIELDS                                        *         
***********************************************************************         
PR45     LTR   R5,R5               TEST FIRST RECORD                            
         BNZ   PR45X                                                            
         LA    R5,1                                                             
         TM    WHEN,X'20'          TEST SOON REPORT                             
         BNO   PR45X                                                            
         L     RF,SPOOLBUF         CHANGE PQ DEFN IN PQ BUFFER                  
         USING PQRECD,RF                                                        
         MVC   SPB(128),0(RF)      SAVE EXISTING BUFFER HEADER DATA             
         MVI   PQCLASS,C'D'                                                     
         MVC   PQDESC,=CL11'STAFF DESC'                                         
         OI    PQATTB,PQATJOBO                                                  
         DROP  RF                                                               
PR45X    EQU   *                                                                
         B     PR20                NEXT RECORD                                  
*                                                                               
PR50     CLI   RPTOPTSH+5,0        END OF REPORT - TEST OPTIONS                 
         BE    PRX                                                              
         CLC   RPTOPTS(6),=C'TEST=2'                                            
         BNE   PRX                                                              
         ICM   RE,15,TWAMASTC      SOON REPORT IF V(MASTER) IS PRESENT          
         BNZ   PRSOON                                                           
                                                                                
***********************************************************************         
* ONLINE NOW REPORT - WHAT HAPPENS TO A SINGLE REPORT?                *         
*                                                                     *         
* IF WE JUST EXIT,REPORT IS CLOSED BY GENCON,AND WE GET:              *         
* REPORT XXX,NNN HAS BEEN CREATED. PAGES=N LINES=NN                   *         
*                                                                     *         
* IF WE CLOSE NOW AND EXIT,THE REPORT IS ON THE PQ,AND WE GET:        *         
* NO REPORT GENERATED FROM REQUEST                                    *         
*                                                                     *         
* ONLINE NOW REPORT - CODE ADDED TO ADD A SECOND REPORT.              *         
* WE MUST CLOSE CURRENT REPORT AND OPEN A NEW ONE.                    *         
* NO NEED TO CLOSE THE SECOND REPORT.                                 *         
* THE SECOND REPORT DETAILS ARE DISPLAYED IN THE HEADLINE.            *         
***********************************************************************         
PRNOW    EQU   *                                                                
         MVI   SPMODE,X'FF'        CLOSE CURRENT REPORT                         
         GOTO1 SPOOL,PARAS,(R8)                                                 
*                                                                               
         MVC   SPOOLID,=C'STF'     SET 3-CHR ID OF NEW REPORT                   
         LA    RF,SPOOLKEY                                                      
         USING PQPLD,RF                                                         
         MVC   PLSUBID,=C'STF'                                                  
         MVC   PLDESC,=CL11'STAFF NOW 2'                                        
         MVI   PLCLASS,C'B'        SET REPORT CLASS                             
         DROP  RF                                                               
*                                                                               
PRNOW1   MVI   SPMODE,X'00'        OPEN NEW REPORT                              
         GOTO1 SPOOL,PARAS,(R8)                                                 
         MVC   SPOOLRPN,SPOOLKEY+19  EXTRACT NEW REPORT NUMBER                  
*                                                                               
PRNOW2   MVC   PRTSTAF,=CL08'ZNOW '                                             
         MVC   PRTNAME,=CL30'NOWNOWNOWNOWNOW'                                   
         MVC   PRTEXTN,=CL04'1234'                                              
         MVC   PRTHOME,=CL12'123-456-7890'                                      
         GOTO1 SPOOL,PARAS,(R8)    PRINT A DUMMY LINE                           
*                                                                               
PRNOW3   B     PRX                 DONT NEED TO CLOSE NEW REPORT                
                                                                                
***********************************************************************         
* OFFLINE SOON REPORT                                                 *         
* TO GET A SECOND REPORT WE MUST CLEAR MCREMPQK AND CHANGE REMOTKEY.  *         
* THE 3-CHR REPORT ID MUST BE DIFFERENT TO THE FIRST REPORT OR ELSE   *         
* REMOTKEY MUST BE CHANGED. THIS EXAMPLE SETS REMOTKEY+9=X,           *         
* MUST THEN OPEN THE SECOND REPORT. NO NEED TO CLOSE IT.              *         
***********************************************************************         
PRSOON   EQU   *                   DONT NEED TO CLOSE WITH SPMODE=X'FF'         
*                                                                               
         ICM   RE,15,TWAMASTC      CLEAR MCREMPQK                               
         JZ    *+2                                                              
         LA    R1,MCREMPQK-MASTD(RE)                                            
         XC    0(17,R1),0(R1)                                                   
*                                                                               
         L     RF,MCVREMOT-MASTD(RE) CHANGE REMOTKEY FOR V(PRINT)               
         USING REMOTED,RF                                                       
         MVC   REMOTJID,=C'STF'      TRY THE SAME 3-CHR ID                      
         MVI   REMOTCLS,C'B'         SET A DIFFERENT CLASS                      
         MVI   REMOTKEY+9,C'X'       TRY TO CHANGE REMOTKEY                     
         MVC   REMOTDSC,=CL11'STAFF SOON2'                                      
         DROP  RF                                                               
*                                                                               
PRSOON1  MVI   SPMODE,X'00'        OPEN NEW REPORT                              
         GOTO1 SPOOL,PARAS,(R8)                                                 
*                                                                               
PRSOON2  MVC   PRTSTAF,=CL08'ZSOON'                                             
         MVC   PRTNAME,=CL30'SOONSOONSOONSOON'                                  
         MVC   PRTEXTN,=CL04'1234'                                              
         MVC   PRTHOME,=CL12'123-456-7890'                                      
         GOTO1 SPOOL,PARAS,(R8)    PRINT A DUMMY LINE                           
*                                                                               
PRSOON3  B     PRX                 DONT NEED TO CLOSE WITH SPMODE=X'FF'         
*                                                                               
PRX      B     XIT                                                              
*                                                                               
         DS    0D                                                               
         DC    C'SPOOLBUF'                                                      
SPB      DC    128X'00'                                                         
         EJECT                                                                  
* HEAD HOOK ROUTINE                                                             
*                                                                               
HDHOOK   NTR1                                                                   
*                                                                               
         MVC   H1+45(19),=C'STAFF RECORD REPORT'                                
         MVI   H2+45,X'BF'         UNDERLINE CHARACTER                          
         MVC   H2+46(19),H2+45                                                  
*                                                                               
         B     HDHOOKX             NO BOXES ON THIS REPORT                      
         ICM   RF,15,ABOX          DO WE HAVE A(BOXES)?                         
         BZ    HDHOOKX             NO                                           
*                                                                               
         USING BOXD,RF             DEFINE BOX AREA                              
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+57,C'B'                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+9,C'C'                                                   
         MVI   BOXCOLS+40,C'C'                                                  
         MVI   BOXCOLS+45,C'C'                                                  
         MVI   BOXCOLS+58,C'R'                                                  
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  RF                                                               
*                                                                               
HDHOOKX  B     XIT                                                              
         EJECT                                                                  
HEDSPECS SSPEC H1,1,AGYNAME                                                     
         SSPEC H2,1,AGYADD                                                      
         SSPEC H1,95,REPORT                                                     
         SSPEC H1,114,REQUESTOR                                                 
         SSPEC H2,95,RUN                                                        
         SSPEC H2,121,PAGE                                                      
         SSPEC H7,2,C'STAFF ID NAME'                                            
         SSPEC H7,42,C'EXTN HOME TEL'                                           
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
VSFMERR  GOTO1 SFMERR                                                           
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
RELO     DS    F                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* DDMASTC                                                                       
* DDREMOTED                                                                     
* DMPRTQD                                                                       
* DMPRTQL                                                                       
* DDBIGBOX                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* CTSFMA4D                                                                      
* CTSFMB4D                                                                      
* CTSFMC4D                                                                      
* CTMSTAFFD                                                                     
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
         EJECT                                                                  
       ++INCLUDE DDREMOTED                                                      
         EJECT                                                                  
       ++INCLUDE DMPRTQD                                                        
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMA4D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMB4D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMC4D                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE CTMSTAFFD                                                      
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
*                                                                               
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
STAFID   DS    CL(L'MSTFID)        SYSTEM ID                                    
SAVEKEY  DS    XL32                GENFILE KEY                                  
*                                                                               
* ON-SCREEN LIST LINE                                                           
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL3                                                              
LSTSTAFF DS    CL8                 STAFF ID                                     
         DS    CL2                                                              
LSTNAME  DS    CL25                FULL NAME                                    
         DS    CL1                                                              
LSTEXTN  DS    CL4                 OFFICE EXTENSION                             
         DS    CL6                                                              
LSTHOME  DS    CL12                HOME PHONE                                   
*                                                                               
* PRINT LINE                                                                    
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    C                   LEFT BOX MARGIN                              
PRTSTAF  DS    CL8                 STAFF ID                                     
         DS    C                                                                
PRTNAME  DS    CL30                NAME                                         
         DS    C                                                                
PRTEXTN  DS    CL4                 OFFICE EXTENSION                             
         DS    C                                                                
PRTHOME  DS    CL12                HOME TELEPHONE                               
         DS    C                   RIGHT BOX MARGIN                             
         DC    X'00'                                                            
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTSFM54   10/21/15'                                      
         END                                                                    
