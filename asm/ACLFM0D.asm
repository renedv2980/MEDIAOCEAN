*          DATA SET ACLFM0D    AT LEVEL 032 AS OF 05/01/02                      
*PHASE T6030DA,+0                                                               
         TITLE 'ACLFM0D - MODULE TO HANDLE RATE ADJUSTMENTS'                    
T6030D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 ADJRTX-ADJRTD,**LFM0D*,R9                                        
         LR    R8,RC                                                            
         USING ADJRTD,R8                                                        
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         MVI   ERROR,X'FF'         INITIALIZE FLAGS                             
         MVI   ANYKEY,C'N'                                                      
         CLI   MODE,BUILDKEY       MUST WE BUILD A KEY ?                        
         BNE   AC300               NO, SEE IF DISPLAY                           
         EJECT                                                                  
***********************************************************************         
*              READ 1R LEDGER AND SAVE HIERARCHY ELEMENT              *         
***********************************************************************         
*                                                                               
         MVC   KEY,SPACES          GET HIERARCHY ELEMENT                        
         MVC   KEY(1),COMPANY       MOVE IN COMPANY                             
         MVC   KEY+1(2),=C'1R'      UNIT AND LEDGER                             
         GOTO1 READ                                                             
         MVI   ELCODE,ACHRELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACHEIRD,R4                                                       
         SR    R3,R3                                                            
         IC    R3,ACHRLEN                                                       
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SAVELED(0),ACHREL   SAVE HIERARCHY ELEMENT                       
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*              VERIFY CLIENT OFFICE AND PRINT NAME                    *         
***********************************************************************         
*                                                                               
         LA    R3,AJRKEY                                                        
         USING PAJRECD,R3                                                       
         XC    AJRKEY,AJRKEY       SET THIS KEY TO BINARY ZEROS                 
         MVI   PAJKTYP,PAJKTYPQ     MOVE IN ADJUST RECORD I.D.                  
         MVC   PAJKCPY,COMPANY      COMPANY                                     
*                                                                               
         OC    AJROFF,SPACES       FILL OFFICE WITH BLANKS                      
         LA    R2,AJROFFH                                                       
         TM    4(R2),X'20'         IF NOT PREVIOUSLY VALIDATED                  
         BO    *+8                  CLEAR NAME FIELDS                           
         BAS   RE,INTFLD1                                                       
         CLI   5(R2),0             OK IF NOTHING ENTERED                        
         BE    AC010                                                            
         CLC   8(3,R2),=C'ALL'     AGENCY LEVEL ?                               
         BE    AC010               YES, LEAVE AS BINARY ZEROS                   
*                                                                               
         TM    COMPSTA4,X'01'      TEST NEW OFFICES                             
         BO    AC002               YES                                          
*                                                                               
         CLI   5(R2),1                                                          
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     XIT                                                              
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY      SET UP KEY WITH COMPANY                      
         MVC   KEY+1(2),=C'2D'      UNIT/LEDGER                                 
         MVC   KEY+3(1),8(R2)       AND OFFICE                                  
         B     AC004                                                            
*                                                                               
AC002    XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING OGRRECD,RE                                                       
         MVI   OGRKTYP,OGRKTYPQ    BUILD PRODUCTION OFFICE RECORD               
         MVI   OGRKSUB,OGRKOFFQ                                                 
         MVC   OGRKCPY,COMPANY                                                  
         MVC   OGRKUNT(2),=C'SJ'                                                
         MVC   OGRKOFC,8(R2)                                                    
         DROP  RE                                                               
*                                                                               
AC004    MVC   PAJKOFF,8(R2)       PUT OFFICE IN ADJUST RECORD KEY ALSO         
         GOTO1 READ                 READ THE OFFICE RECORD                      
         GOTO1 NAMOUT               AND GET THE NAME                            
         EJECT                                                                  
***********************************************************************         
*              VERIFY CLIENT, PRODUCT & JOB AND PRINT NAMES           *         
***********************************************************************         
*                                                                               
AC010    OI    AJROFFH+4,X'20'     INDICATE OFFICE VALIDATED                    
         OC    AJRCLI,SPACES       FILL CLIENT WITH BLANKS                      
         LA    R2,AJRCLIH                                                       
         TM    4(R2),X'20'         IF NOT PREVIOUSLY VALIDATED                  
         BO    *+8                 CLEAR NAME FIELDS                            
         BAS   RE,INTFLD2                                                       
         CLI   5(R2),0             OK IF NOTHING ENTERED                        
         BE    AC018                                                            
*                                                                               
         MVC   KEY,SPACES          CLEAR THE KEY                                
         MVC   KEY(1),COMPANY       MOVE IN COMPANY                             
         MVC   KEY+1(2),=C'SJ'      AND UNIT AND LEDGER                         
         LA    R6,KEY+3            MOVE CLIENT TO KEY                           
         ZIC   R5,5(R2)                                                         
         BCTR  R5,0                                                             
         EX    R5,MVCDATA                                                       
         MVC   PAJKCLI,8(R2)        AND ADJUST KEY ALSO                         
         GOTO1 READ                READ THE RECORD                              
         GOTO1 NAMOUT               AND GET THE NAME                            
*                                                                               
         LR    R5,R6              SAVE CURRENT ADDRESS OF KEY                   
         LA    R6,IO                                                            
         AH    R6,DATADISP                                                      
*                                                                               
AC012    CLI   0(R6),0            FIND THE X'24' ELEMENT                        
         BE    AC014                                                            
         CLI   0(R6),PPRELQ                                                     
         BE    AC016                                                            
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     AC012                                                            
*                                                                               
AC014    MVI   ERROR,X'FE'        ERROR, OFFICE INCORRECT                       
         MVC   LOGHEAD(31),=CL31'** ERROR ** INVALID OFFICE CODE'               
         OI    6(R2),X'40'                                                      
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
*                                                                               
         USING PPRELD,R6                                                        
AC016    MVC   PAJKOFF,PPRGAOFF   MOVE OFFICE TO CHARGE RECORD                  
         LR    R6,R5               GET BACK TO WHERE WE WERE                    
         LA    R6,3(0,R6)          BUMP UP POINTERS                             
*                                                                               
AC018    OI    AJRCLIH+4,X'20'     INDICATE CLIENT IS VALID                     
         OC    AJRPRO,SPACES       FILL PRODUCT WITH BLANKS                     
         LA    R2,AJRPROH                                                       
         TM    4(R2),X'20'         IF NOT PREVIOUSLY VALIDATED                  
         BO    *+8                  CLEAR NAME FIELDS                           
         BAS   RE,INTFLD3                                                       
         CLI   5(R2),0             OK IF NOT ENTERED                            
         BE    AC024                                                            
*                                                                               
         LA    R2,AJRCLIH          IF PRODUCT ENTERED                           
         CLI   5(R2),0              CLIENT MUST ALSO BE THERE                   
         BNE   *+12                                                             
         MVI   ERROR,NEEDCLI                                                    
         B     XIT                                                              
*                                                                               
         LA    R2,AJRPROH          GET BACK TO WHERE WE WERE                    
         ZIC   R5,5(R2)            MOVE PRODUCT TO KEY                          
         BCTR  R5,0                                                             
         EX    R5,MVCDATA                                                       
         MVC   PAJKPRO,8(R2)        AND ADJUST KEY                              
         GOTO1 READ                READ THE RECORD                              
         GOTO1 NAMOUT               AND GET THE NAME                            
*                                                                               
         LR    R5,R6              SAVE CURRENT ADDRESS OF KEY                   
         LA    R6,IO                                                            
         AH    R6,DATADISP                                                      
*                                                                               
AC020    CLI   0(R6),0            FIND THE X'24' ELEMENT                        
         BE    AC024                                                            
         CLI   0(R6),PPRELQ                                                     
         BE    AC022                                                            
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     AC020                                                            
*                                                                               
AC022    CLC   PPRGAOFF,SPACES                                                  
         BNH   *+10                                                             
         MVC   PAJKOFF,PPRGAOFF                                                 
*                                                                               
AC024    OI    AJRPROH+4,X'20'     INDICATE PRODUCT IS VALID                    
         CLC   PAJKOFF,SPACES      DO WE HAVE AN OFFICE?                        
         BNH   AC032                                                            
         LA    R2,AJROFFH                                                       
         CLC   AJROFF(L'PAJKOFF),PAJKOFF  OK IF THEY MATCH                      
         BE    AC026                                                            
         CLI   5(R2),0             IF OFFICE ENTERED, THERE IS                  
         BNE   AC014                ERROR                                       
*                                                                               
AC026    MVC   SAVEKEY,KEY        HOLD KEY SO FAR                               
         TM    COMPSTA4,X'01'      TEST NEW OFFICES                             
         BO    AC028               YES                                          
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'2D'                                                  
         MVC   KEY+3(1),PAJKOFF                                                 
         B     AC030                                                            
*                                                                               
AC028    XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING OGRRECD,RE                                                       
         MVI   OGRKTYP,OGRKTYPQ    BUILD PRODUCTION OFFICE RECORD               
         MVI   OGRKSUB,OGRKOFFQ                                                 
         MVC   OGRKCPY,COMPANY                                                  
         MVC   OGRKUNT(2),=C'SJ'                                                
         MVC   OGRKOFC,PAJKOFF                                                  
         DROP  RE                                                               
*                                                                               
AC030    MVC   AJROFF,SPACES       PUT OFFICE ON SCREEN                         
         MVC   AJROFF(L'PAJKOFF),PAJKOFF                                        
         OI    AJROFFH+6,X'80'     AND TRANSMIT AND VALIDATE                    
         OI    AJROFFH+4,X'20'                                                  
         GOTO1 READ                READ THE OFFICE RECORD                       
         MVC   AJRONAM,SPACES      CLEAR OFFICE NAME                            
         GOTO1 NAMOUT              GET THE NAME                                 
         OI    AJRONAMH+6,X'80'                                                 
         MVC   KEY,SAVEKEY                                                      
*                                                                               
AC032    LR    R6,R5               GET BACK TO WHERE WE WERE                    
         LA    R6,3(0,R6)          BUMP UP POINTERS                             
         OC    AJRJOB,SPACES       FILL JOB WITH BLANKS                         
         LA    R2,AJRJOBH                                                       
         TM    4(R2),X'20'         IF NOT PREVIOUSLY VALIDATED                  
         BO    *+8                  CLEAR NAME FIELDS                           
         BAS   RE,INTFLD4                                                       
         CLI   5(R2),0             OK IF NOT ENTERED                            
         BE    AC050                                                            
*                                                                               
         LA    R2,AJRPROH          IF JOB ENTERED                               
         CLI   5(R2),0              PRODUCT MUST ALSO BE THERE                  
         BNE   *+12                                                             
         MVI   ERROR,NEEDPRO                                                    
         B     XIT                                                              
*                                                                               
         LA    R2,AJRJOBH          GET BACK TO WHERE WE WERE                    
         ZIC   R5,5(R2)            MOVE JOB TO KEY                              
         BCTR  R5,0                                                             
         EX    R5,MVCDATA                                                       
         MVC   PAJKJOB,8(R2)        AND ADJUST KEY                              
         GOTO1 READ                READ THE RECORD                              
         GOTO1 NAMOUT               AND GET THE NAME                            
         EJECT                                                                  
***********************************************************************         
*              VERIFY THE OFFICE AND GET THE NAME                     *         
***********************************************************************         
*                                                                               
         USING ACHEIRD,R4                                                       
AC050    OI    AJRJOBH+4,X'20'     INDICATE JOB IS VALID                        
         LA    R4,SAVELED                                                       
         LA    R2,AJRDOFH                                                       
         OC    AJRDOF,SPACES       FILL OFFICE WITH BLANKS                      
         TM    4(R2),X'20'         IF NOT PREVIOUSLY VALIDATED                  
         BO    *+8                  CLEAR NAME FIELDS                           
         BAS   RE,INTFLD5                                                       
*                                                                               
         MVC   KEY,SPACES          CLEAR THE KEY AGAIN                          
         MVC   KEY(1),COMPANY       MOVE IN COMPANY                             
         MVC   KEY+1(2),=C'1R'      UNIT AND LEDGER                             
         LA    R6,KEY+3             AND DEPT                                    
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,1,5(R2)                                                       
         BZ    AC100                                                            
         LA    R2,AJRDEPH          MUST HAVE A DEPARTMENT ALSO                  
         CLI   5(R2),0                                                          
         BE    MISSIT                                                           
         LA    R2,AJRDOFH          GET ADDRESS OF OFFICE AGAIN                  
         SR    R7,R7                                                            
         IC    R7,ACHRLEVA                                                      
         CR    R5,R7               CHECK LENGTH OF INPUT                        
         BH    LONGERR                                                          
*                                                                               
         BCTR  R7,0                                                             
         EX    R7,MVCDATA          MOVE TO 1R FOR HIERARCHY LENGTH              
         LA    R7,1(0,R7)          BUMP R7 BACK UP                              
         MVC   PAJKDOF,8(R2)       MOVE TO X'2A' RECORD ALSO                    
*                                                                               
         GOTO1 READ                NO, READ THE RECORD                          
         GOTO1 NAMOUT               AND GET THE NAME                            
         EJECT                                                                  
***********************************************************************         
*              VERIFY THE DEPARTMENT AND GET THE NAME                 *         
***********************************************************************         
*                                                                               
AC100    OI    AJRDOFH+4,X'20'     INDICATE OFFICE IS VALID                     
         LA    R6,0(R7,R6)         BUMP UP POINTERS                             
         LA    R2,AJRDEPH                                                       
         OC    AJRDEP,SPACES       FILL DEPT WITH BLANKS                        
         TM    4(R2),X'20'         IF NOT PREVIOUSLY VALIDATED                  
         BO    *+8                  CLEAR NAME FIELDS                           
         BAS   RE,INTFLD6                                                       
*                                                                               
         SR    R7,R7               GET LENGTH OF THIS LEVEL                     
         IC    R7,ACHRLEVB                                                      
         SR    RF,RF                                                            
         IC    RF,ACHRLEVA                                                      
         SR    R7,RF                                                            
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,1,5(R2)          ANYTHING ENTERED ?                           
         BZ    AC104               NO                                           
         LA    R2,AJRDOFH          YES, MUST HAVE OFFICE                        
         CLI   5(R2),0                                                          
         BE    MISSIT                                                           
         LA    R2,AJRDEPH                                                       
         CR    R5,R7               CHECK LENGTH                                 
         BH    LONGERR                                                          
*                                                                               
         BCTR  R7,0                                                             
         EX    R7,MVCDATA          MOVE TO 1R FOR HIERARCHY LENGTH              
         LA    R7,1(0,R7)          BUMP R7 BACK UP                              
         MVC   PAJKDEP,8(R2)       MOVE TO X'2A' RECORD ALSO                    
*                                                                               
         GOTO1 READ                NO, READ RECORD AND                          
         GOTO1 NAMOUT               GET THE NAME                                
         EJECT                                                                  
***********************************************************************         
*              VERIFY THE SUB-DEPARTMENT AND GET THE NAME             *         
***********************************************************************         
*                                                                               
AC104    LA    R6,0(R7,R6)         BUMP UP POINTERS                             
         OI    AJRDEPH+4,X'20'      INDICATE FIELD IS VALIDATED                 
         LA    R2,AJRSDH                                                        
         OC    AJRSD,SPACES        FILL SUB-DEPT WITH BLANKS                    
         TM    4(R2),X'20'         IF NOT PREVIOUSLY VALIDATED                  
         BO    *+8                  CLEAR THE NAME FIELDS                       
         BAS   RE,INTFLD7                                                       
*                                                                               
         SR    R7,R7                                                            
         IC    R7,ACHRLEVC         GET LENGTH OF LEVEL                          
         SR    RF,RF                                                            
         IC    RF,ACHRLEVB                                                      
         SR    R7,RF                                                            
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,1,5(R2)          OK IF NOTHING ENTERED                        
         BZ    AC106                                                            
         CLI   AJRDEPH+5,0         IF SUB-DEPT ENTERED                          
         BE    MISSIT               OFFICE/DEPT MUST ALSO BE THERE              
*                                                                               
         CR    R5,R7               CHECK LENGTH                                 
         BH    LONGERR                                                          
         BCTR  R7,0                                                             
         EX    R7,MVCDATA          MOVE TO 1R FOR HIERARCHY LENGTH              
         LA    R7,1(0,R7)          BUMP R7 BACK UP                              
         MVC   PAJKSUB,8(R2)       MOVE TO X'2A' RECORD ALSO                    
*                                                                               
         GOTO1 READ                NO, READ THE RECORD                          
         GOTO1 NAMOUT               AND GET THE NAME                            
         EJECT                                                                  
***********************************************************************         
*              VERIFY THE STAFF AND GET THE NAME                      *         
***********************************************************************         
*                                                                               
AC106    LA    R6,0(R7,R6)         BUMP UP POINTERS                             
         OI    AJRSDH+4,X'20'      INDICATE FIELD VALIDATED                     
         LA    R2,AJRSTAH                                                       
         OC    AJRSTA,SPACES       FILL STAFF WITH BLANKS                       
         TM    4(R2),X'20'         IF NOT PREVIOUSLY VALIDATED                  
         BO    *+8                  CLEAR NAME FIELDS                           
         BAS   RE,INTFLD8                                                       
*                                                                               
         SR    R7,R7               GET LENGTH OF LEVEL                          
         IC    R7,ACHRLEVD                                                      
         SR    RF,RF                                                            
         IC    RF,ACHRLEVC                                                      
         SR    R7,RF                                                            
*                                                                               
         CLI   5(R2),0             OK IF NOTHING ENTERED                        
         BE    AC200                                                            
         CLI   AJRSDH+5,0          IF STAFF ENTERED                             
         BE    MISSIT               SUB-DEPT MUST ALSO BE THERE                 
         ZIC   R5,5(R2)             AND MOVE SUB-DEPT TO KEY                    
         CR    R5,R7               CHECK LENGTH                                 
         BH    LONGERR                                                          
*                                                                               
         BCTR  R7,0                                                             
         EX    R7,MVCDATA          MOVE TO 1R FOR HIERARCHY LENGTH              
         MVC   PAJKSTF,8(R2)       MOVE TO X'2A' RECORD ALSO                    
*                                                                               
         GOTO1 READ                NO, READ RECORD                              
         GOTO1 NAMOUT               AND GET NAME                                
         EJECT                                                                  
***********************************************************************         
*              VERIFY THE TASK AND PRINT NAME                         *         
***********************************************************************         
*                                                                               
AC200    OI    AJRSTAH+4,X'20'     INDICATE FIELD VALIDATED                     
         OC    AJRTASK,SPACES      MAKE FILL CHARACTER BLANKS                   
         LA    R2,AJRTASKH                                                      
         TM    4(R2),X'20'         IF NOT PREVIOUSLY VALIDATED                  
         BO    *+8                  CLEAR NAME FIELDS                           
         BAS   RE,INTFLD9                                                       
         CLI   5(R2),0             OK IF NOTHING ENTERED                        
         BE    AC202                                                            
*                                                                               
         MVC   KEY,SPACES          SET THIS KEY TO BLANKS                       
         MVI   KEY,X'0A'            MOVE IN TASK RECORD I.D.                    
         MVC   KEY+1(1),COMPANY     COMPANY                                     
         MVC   KEY+2(2),=C'SJ'      UNIT/LEDGER                                 
         MVC   KEY+4(2),8(R2)       AND TASK                                    
         MVC   PAJKTSK,8(R2)       PUT TASK IN ADJUST RECORD KEY                
         GOTO1 READ                READ THE RECORD                              
         MVI   ELCODE,WCOELQ        AND GET NAME ELEMENT                        
         BAS   RE,GETELIO                                                       
         USING WCOELD,R4                                                        
         MVC   AJRTNAM,WCODESC      MOVE IN THE NAME                            
         DROP  R4                                                               
*                                                                               
AC202    OI    AJRTASKH+4,X'20'    INDICATE TASK VALIDATED                      
*                                                                               
         OC    PAJKOFF(L'PAJKEY-(PAJKOFF-PAJKTYP)),PAJKOFF                      
         BNZ   AC204                                                            
         LA    R2,AJROFFH          NO KEY, MUST HAVE ENTERED ALL                
         GOTO1 ANY                                                              
*                                                                               
AC204    MVC   KEY,AJRKEY          SETUP KEY                                    
         LA    R2,AJROFFH          SET UP CURSOR FOR ERROR                      
         CLI   LOGACT,C'N'         TEST IF ADDING NEW RECORD                    
         BNE   AC206               NO                                           
*                                                                               
         XC    IO2(42),IO2                                                      
         BAS   RE,SETUP                                                         
         GOTO1 HIGH                SEE IF RECORD EXISTS                         
         BAS   RE,RESET                                                         
         NI    4(R2),X'DF'         RESET VALIDATION BIT                         
         CLC   KEYSAVE,KEY         EXIT IF IT DOESN'T EXIST                     
         BNE   AC206                                                            
         MVI   ERROR,RECONFLE      SET ERROR CODE IF IT DOES                    
*                                                                               
AC206    B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* DISPLAY A RECORD.                                                  *          
**********************************************************************          
*                                                                               
AC300    CLI   MODE,DSPLYREC       IF NOT DISPLAY, SEE WHAT TO DO               
         BNE   AC400                                                            
         BAS   RE,ACDSPLY                                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD A RECORD FOR NEW/CHANGE.                                      *         
***********************************************************************         
*                                                                               
AC400    CLI   MODE,BUILDREC       IF NOT BUILD A RECORD, GET OUT               
         BNE   XIT                                                              
         CLI   LOGACT,C'N'         BYPASS DELETE IF THIS IS 'NEW'               
         BE    AC410                OR NOT DELETE                               
         MVC   KEY,AJRKEY                                                       
         XC    IO2(42),IO2         SETUP AND GET RECORD                         
         MVI   UPDATE,C'Y'          FOR UPDATE                                  
         LA    R2,AJRDEPH                                                       
         BAS   RE,SETUP                                                         
         GOTO1 READ                                                             
         BAS   RE,RESET                                                         
*                                                                               
         LA    R6,IO                                                            
         LA    R2,AJRDAT1H                                                      
         CLC   8(6,R2),=C'DELETE'                                               
         BNE   AC405                                                            
         USING ACCRECD,R6                                                       
         OI    ACCRSTA,X'80'       FLAG RECORD FOR DELETE                       
         LA    R4,IO2                                                           
         LH    R5,ACCRLEN                                                       
         LR    R7,R5                                                            
         MVCL  R4,R6               MOVE FROM IO TO IO2                          
         B     AC480                                                            
*                                                                               
AC405    LA    R4,IO2                                                           
         LH    R5,ACCRLEN                                                       
         LR    R7,R5                                                            
         MVCL  R4,R6               MOVE FROM IO TO IO2                          
         GOTO1 REMANEL,DMCB,(X'53',0)                                           
         B     AC415                                                            
*                                                                               
AC410    LA    R4,IO2              CLEAR RECORD IN IO2                          
         LA    R5,IOLENQ                                                        
         LA    R6,*                                                             
         XR    R7,R7                                                            
         MVCL  R4,R6                                                            
         LA    R6,IO2                                                           
         XC    IO2(42),IO2                                                      
         MVC   KEY,AJRKEY         BUILD NEW RECORD                              
         MVC   ACCKEY(32),KEY                                                   
         MVC   ACCRLEN,DATADISP                                                 
         GOTO1 STATIN                                                           
         DROP  R6                                                               
*                                                                               
AC415    LA    R5,AJRDAT1H         POINT TO FIRST FIELD                         
         USING LND,R5                                                           
AC420    LA    R4,ELEMENT          GET ADDRESS OF WORK AREA                     
         USING TCIELD,R4                                                        
         XC    ELEMENT,ELEMENT     CLEAR THE ELEMENT                            
         MVI   TCIEL,TCIELQ        MOVE IN ELEMENT CODE                         
         MVI   TCILN,TCILNQ         AND THE LENGTH                              
         LA    R2,LNDRATH          GET FIRST FIELD                              
         GOTO1 ANY                 EXIT IF NOTHING THERE                        
         SR    R6,R6                                                            
         IC    R6,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(4,8(R2)),(R6)                                      
         CLI   DMCB,0                                                           
         BNE   AC430                                                            
         L     R1,DMCB+4                                                        
         CVD   R1,DUB                                                           
         CP    DUB,=P'9999999'     MAXIMUM IS 999.9999                          
         BNH   AC440               TOO HIGH - ERROR                             
*                                                                               
AC430    MVI   ERROR,CASHERR       INDICATE ERROR                               
         B     XIT                  AND EXIT                                    
*                                                                               
AC440    CLI   LNDSEQ,X'00'                                                     
         BNE   AC445                                                            
         L     R3,LASTNUM                                                       
         LA    R3,1(R3)                                                         
         STC   R3,LNDSEQ                                                        
         ST    R3,LASTNUM                                                       
*                                                                               
AC445    MVC   TCICM(1),LNDSEQ     GET SEQUENCE NUMBER                          
         ZAP   TCIAMT,DUB          PUT IT INTO RECORD                           
         LA    R2,LNDDATH                                                       
         CLI   5(R2),0             ADD ELEMENT AND GET NEXT ENTRY               
         BE    AC450                IF NO DATE                                  
         GOTO1 DATVAL,DMCB,(0,LNDDAT),WORK                                      
         OC    DMCB,DMCB           INDICATE ERROR AND EXIT IF                   
         BNZ   *+12                INVALID                                      
         MVI   ERROR,DATERR                                                     
         B     XIT                                                              
*                                                                               
**********************************************************************          
*          CHECK FOR DUPLICATE DATES                                 *          
**********************************************************************          
         GOTO1 DATCON,DMCB,(0,WORK),(1,TCIDTE)     UPDATE RECORD                
*                                                                               
AC450    DS    0H                                                               
         GOTO1 DATCON,DMCB,(1,TCIDTE),(8,MYDATE)   CONVERT DATE                 
         LA    R7,AJRDAT1H         POINT TO FIRST DATE                          
*                                                                               
AC455    DS    0H                                                               
         CLI   21(R7),0            IS THERE A RATE?                             
         BE    AC458               NO                                           
         CR    R7,R5               CURRENT DATE?                                
         BE    AC458               YES                                          
         CLC   MYDATE,8(R7)        DUPLICATE DATE?                              
         BNE   *+12                NO                                           
*                                                                               
AC456    MVI   ERROR,DUPLCATE                                                   
         B     XIT                                                              
         LR    R3,R2                                                            
         LR    R2,R7                                                            
         GOTO1 DATVAL,DMCB,(0,8(R7)),WORK                                       
         CLI   5(R7),0                                                          
         BE    AC457                                                            
         OC    DMCB,DMCB           INDICATE ERROR AND EXIT IF                   
         BNZ   *+12                INVALID                                      
         MVI   ERROR,DATERR                                                     
         B     XIT                                                              
*                                                                               
AC457    DS    0H                                                               
         LR    R2,R3               RESTORE                                      
         GOTO1 DATCON,DMCB,(0,WORK),(8,MYDATE2)                                 
         CLC   MYDATE,MYDATE2      DUPLICATE DATE?                              
         BE    AC456               YES                                          
*                                                                               
AC458    LA    R7,LNDLNG(R7)       GO TO NEXT DATE                              
         CLC   AJRENDH,0(R7)       END OF DATES?                                
         BNE   AC455                                                            
*                                                                               
         USING COMFACSD,R3                                                      
AC460    L     R3,COMFACS                                                       
         GOTO1 CHELLO,DMCB,(C'P',=C'ACCOUNT '),IO2,ELEMENT                      
         CLI   DMCB+12,0                                                        
         BE    AC470                                                            
         DC    H'0'                                                             
*                                                                               
AC470    LA    R5,LNDLNG(R5)        AND LOOK AT NEXT                            
         CLI   0(R5),9             IF NOT MORE DATA, ADD RECORD                 
         BE    AC480                                                            
         CLI   LNDDATH+5,0         GO HANDLE DATE                               
         BNE   AC420                                                            
         CLI   LNDRATH+5,0         OR RATE                                      
         BNE   AC420                                                            
         B     AC470               OR TRY NEXT ENTRY                            
         DROP  R5                                                               
*                                                                               
AC480    OI    DMINBTS,X'88'       LOOK AT DELETED RECORDS                      
         MVC   KEY,IO2                                                          
         BAS   RE,SETUP            SETUP AND READ RECORD                        
         GOTO1 HIGH                                                             
         BAS   RE,RESET                                                         
         NI    DMINBTS,X'FF'-X'88'                                              
         CLC   KEYSAVE,KEY         IF NOT THERE, ADD IT                         
         BNE   AC490                                                            
         BAS   RE,SETUP1                                                        
         GOTO1 PUTREC              OTHERWISE, UPDATE IT                         
         BAS   RE,RESET                                                         
         MVI   ERROR,X'FF'                                                      
         B     AC500                                                            
*                                                                               
AC490    BAS   RE,SETUP1                                                        
         GOTO1 ADDREC                                                           
         BAS   RE,RESET                                                         
*                                                                               
AC500    BAS   RE,ACDSPLY          RE-DISPLAY RECORD                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO DISPLAY DATA                                *         
***********************************************************************         
*                                                                               
ACDSPLY  NTR1                                                                   
         TWAXC AJRDAT1H,AJRENDH    CLEAR WORK AREA                              
         LA    R2,AJROFFH                                                       
         MVC   KEY,AJRKEY          SETUP AND GET RECORD                         
         BAS   RE,SETUP                                                         
         GOTO1 READ                                                             
         BAS   RE,RESET                                                         
*                                                                               
         USING TCIELD,R4                                                        
XSORTEL  SR    R5,R5                                                            
         MVI   ELCODE,TCIELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   XIT                                                              
         ST    R4,XSORTADD                                                      
         B     XBUMP                                                            
*                                                                               
XGETNXT  BAS   RE,NEXTEL                                                        
         BNE   QSORTIT                                                          
*                                                                               
XBUMP    LA    R5,1(R5)                                                         
         CLI   TCICM,X'00'         DO WE HAVE ASEQUENCE NUMBER ?                
         BE    SETR5               NO, SET ONE                                  
         CLI   TCICM,X'40'         YES, SURE IT'S A NUMBER ?                    
         BL    *+8                 YES                                          
*                                                                               
SETR5    STC   R5,TCICM                                                         
         B     XGETNXT                                                          
*                                                                               
QSORTIT  ST    R5,LASTNUM          SAVE NUMBER FOR NEXT RECORD                  
         L     R2,XSORTADD                                                      
         GOTO1 CALLOV,DMCB,0,X'D9000A50'                                        
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R2),(R5),19,4,12                                      
         LA    R2,AJRDAT1H         POSITION CURSOR FOR AMEND                    
         LA    R5,AJRDAT1H         ADDRESS FIRST FIELD                          
         USING LND,R5                                                           
         LA    R4,IO               GET SAVED ADJUST RECORD                      
         AH    R4,DATADISP         ADD DISPLACEMENT OF ELEMENT                  
*                                                                               
QSORT2   CLI   0(R4),X'53'         LOOK FOR X'53' ELEMENT OR                    
         BE    QSORT6               END-OF-RECORD                               
         CLI   0(R4),0                                                          
         BE    XIT                                                              
*                                                                               
QSORT4   ZIC   R0,1(R4)            NOT RIGHT ONE, BUMP ADDRESS                  
         AR    R4,R0                AND KEEP LOOKING                            
         B     QSORT2                                                           
*                                                                               
QSORT6   EDIT  TCIAMT,(9,LNDRAT),4,FLOAT=-                                      
         MVC   LNDSEQ,TCICM                                                     
         OC    TCIDTE,TCIDTE        IF NO EFFECTIVE DATE, GET                   
         BZ    QSORT8               THE NEXT ENTRY                              
         GOTO1 DATCON,DMCB,(1,TCIDTE),(8,LNDDAT)                                
*                                                                               
QSORT8   LA    R5,LNDLNG(R5)       DONE, GET NEXT ENTRY AND                     
         B     QSORT4               ELEMENT                                     
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALIZE NAME FIELDS AND FIELD HEADERS                            *         
***********************************************************************         
*                                                                               
INTFLD1  MVC   AJRONAM,SPACES      CLEAR OFFICE NAME                            
         OI    AJRONAMH+6,X'80'     TRANSMIT                                    
         OI    6(R2),X'80'          RETRANSMIT CODE ALSO                        
         NI    AJRCLIH+4,X'DF'      TURN OFF CLIENT BIT                         
*                                                                               
INTFLD2  MVC   AJRCNAM,SPACES      CLEAR CLIENT NAME                            
         OI    AJRCNAMH+6,X'80'     TRANSMIT                                    
         OI    6(R2),X'80'          RETRANSMIT CODE ALSO                        
         NI    AJRPROH+4,X'DF'      TURN OFF PRODUCT BIT                        
*                                                                               
INTFLD3  MVC   AJRPNAM,SPACES      CLEAR PRODUCT NAME                           
         OI    AJRPNAMH+6,X'80'     TRANSMIT                                    
         OI    6(R2),X'80'          RETRANSMIT CODE ALSO                        
         NI    AJRJOBH+4,X'DF'      TURN OFF JOB BIT                            
*                                                                               
INTFLD4  MVC   AJRJNAM,SPACES      CLEAR JOB NAME                               
         OI    AJRJNAMH+6,X'80'     TRANSMIT                                    
         OI    6(R2),X'80'          RETRANSMIT CODE ALSO                        
         NI    AJRDOFH+4,X'DF'      TURN OFF OFFICE BIT                         
*                                                                               
INTFLD5  MVC   AJRDONM,SPACES      CLEAR OFFICE NAME                            
         OI    AJRDONMH+6,X'80'     TRANSMIT                                    
         OI    6(R2),X'80'          RETRANSMIT CODE ALSO                        
         NI    AJRDEPH+4,X'DF'      TURN OFF DEPARTMENT BIT                     
*                                                                               
INTFLD6  MVC   AJRDNAM,SPACES      CLEAR DEPT NAME                              
         OI    AJRDNAMH+6,X'80'     TRANSMIT                                    
         OI    6(R2),X'80'          RETRANSMIT CODE ALSO                        
         NI    AJRSDH+4,X'DF'       TURN OFF SUB-DEPT BIT                       
*                                                                               
INTFLD7  MVC   AJRSNAM,SPACES      CLEAR SUB-DEPT NAME                          
         OI    AJRSNAMH+6,X'80'     TRANSMIT                                    
         OI    6(R2),X'80'          RETRANSMIT CODE ALSO                        
         NI    AJRSTAH+4,X'DF'      TURN OFF STAFF BIT                          
*                                                                               
INTFLD8  MVC   AJRSTNO,SPACES      CLEAR STAFF NAME                             
         OI    AJRSTNOH+6,X'80'     TRANSMIT                                    
         OI    6(R2),X'80'          RETRANSMIT CODE ALSO                        
         NI    AJRTASKH+4,X'FD'     TURN OFF TASK BIT                           
*                                                                               
INTFLD9  MVC   AJRTNAM,SPACES      CLEAR TASK NAME                              
         OI    AJRTNAMH+6,X'80'     INDICATE TRANSMIT FIELD                     
         OI    6(R2),X'80'          RETRANSMIT CODE ALSO                        
         MVI   ANYKEY,C'Y'                                                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* MISCELLANEOUS ROUTINES                                              *         
***********************************************************************         
*                                                                               
GETELIO  LA    R4,IO                                                            
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
MISSIT   MVI   ERROR,MISSING                                                    
         B     XIT                                                              
*                                                                               
LONGERR  MVI   ERROR,FLD2LONG                                                   
*                                                                               
XIT      XIT1  REGS=(R2)                                                        
*                                                                               
MVCDATA  MVC   0(0,R6),8(R2)                                                    
*                                                                               
SETUP    XC    IO(49),IO                                                        
         MVC   IO(32),KEY                                                       
*                                                                               
SETUP1   OI    RECOREL,FULLKEY                                                  
         BR    RE                                                               
*                                                                               
RESET    NI    RECOREL,X'FF'-FULLKEY                                            
         BR    RE                                                               
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMF2D                                                       
AJRKEY   DS    CL32                                                             
LASTNUM  DS    X                                                                
MYDATE   DS    XL8                                                              
MYDATE2  DS    XL8                                                              
         EJECT                                                                  
* ACLFMEQU                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACLFMEQU                                                       
         PRINT ON                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE ACLFMWORK                                                      
         EJECT                                                                  
ADJRTD   DSECT                                                                  
ELCODE   DS    C                                                                
FULLKEY  EQU   X'08'                                                            
SAVEKEY  DS    CL32                                                             
XSORTADD DS    F                                                                
SAVELED  DS    CL(ACHRLENQ)                                                     
ADJRTX   DS    0C                                                               
         EJECT                                                                  
LND      DSECT                                                                  
LNDDATH  DS    CL8                 DATE HEADER                                  
LNDDAT   DS    CL8                 DATE FIELD                                   
LNDRATH  DS    CL8                 RATE HEADER                                  
LNDRAT   DS    CL9                 RATE FIELD                                   
LNDSEQH  DS    CL8                 SEQUENCE HEADER                              
LNDSEQ   DS    CL1                 SEQUENCE FIELD                               
LNDLNG   EQU   *-LND                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032ACLFM0D   05/01/02'                                      
         END                                                                    
