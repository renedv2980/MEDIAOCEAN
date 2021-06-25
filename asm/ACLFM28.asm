*          DATA SET ACLFM28    AT LEVEL 085 AS OF 01/21/04                      
*PHASE T60328A,+0                                                               
*INCLUDE ACRAPPER                                                               
T60328   CSECT                                                                  
         TITLE 'T60328 - MODULE TO HANDLE CHARGE ELEMENTS'        '             
         PRINT NOGEN                                                            
         NMOD1 100,**LFM28*,R9                                                  
         LR    R8,RC                                                            
         USING CHARGED,R8                                                       
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
*                                                                               
         MVI   ERROR,X'FF'         INITIALIZE FLAGS                             
         MVI   ANYKEY,C'N'                                                      
         CLI   MODE,BUILDKEY       MUST WE BUILD A KEY ?                        
         BNE   AC200               NO, SEE IF DISPLAY                           
         EJECT                                                                  
***********************************************************************         
* FORMAT KEY FOR CONTRA ACCOUNT RECORD (DEPT, SUB-DEPT, STAFF)        *         
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
* VERIFY THE OFFICE AND GET THE NAME.                                 *         
***********************************************************************         
*                                                                               
         LA    R2,CHGOFFH          AN OFFICE MUST BE ENTERED                    
         CLI   5(R2),0             OR IT IS AN ERROR                            
         BNE   AC000                                                            
         BAS   RE,INTFLD0                                                       
         MVI   ERROR,242                                                        
         B     XIT                                                              
*                                                                               
         USING PCHRECD,R3                                                       
         USING ACHEIRD,R4                                                       
AC000    LA    R3,CHGKEY                                                        
         LA    R4,SAVELED                                                       
         OC    CHGOFF,SPACES       FILL OFFICE WITH BLANKS                      
         TM    4(R2),X'20'         IF NOT PREVIOUSLY VALIDATED                  
         BO    *+8                  CLEAR NAME FIELDS                           
         BAS   RE,INTFLD0                                                       
*                                                                               
         MVC   KEY,SPACES          CLEAR THE KEY AGAIN                          
         MVC   KEY(1),COMPANY       MOVE IN COMPANY                             
         MVC   KEY+1(2),=C'1R'      UNIT AND LEDGER                             
         XC    CHGKEY,CHGKEY       SET THIS KEY TO BINARY ZEROS                 
         MVI   PCHKTYP,PCHKTYPQ     MOVE IN CHARGE RECORD I.D.                  
         MVC   PCHKCPY,COMPANY      COMPANY                                     
         LA    R6,KEY+3            GET PLACE FOR OFFICE                         
*                                                                               
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         SR    R7,R7                                                            
         IC    R7,ACHRLEVA                                                      
         CR    R5,R7               CHECK LENGTH OF INPUT                        
         BH    LONGERR                                                          
*                                                                               
         BCTR  R7,0                                                             
         EX    R7,MVCDATA          MOVE TO 1R FOR HIERARCHY LENGTH              
         LA    R7,1(0,R7)          BUMP R7 BACK UP                              
         MVC   PCHKDOF,8(R2)       MOVE TO X'2A' RECORD ALSO                    
*                                                                               
         GOTO1 READ                READ RECORD AND                              
         GOTO1 NAMOUT               GET THE NAME                                
         EJECT                                                                  
***********************************************************************         
* VERIFY THE DEPARTMENT AND GET THE NAME.                             *         
***********************************************************************         
*                                                                               
         LA    R6,0(R7,R6)         NEXT POSITION IN 1R RECORD                   
         OI    4(R2),X'20'         OFFICE IS VALIDATED                          
*                                                                               
         LA    R2,CHGDEPH          DEPARTMENT MUST BE ENTERED                   
         OC    CHGDEP,SPACES       FILL DEPT WITH BLANKS                        
         TM    4(R2),X'20'         IF NOT PREVIOUSLY VALIDATED                  
         BO    *+8                  CLEAR NAME FIELDS                           
         BAS   RE,INTFLD1                                                       
*                                                                               
         SR    R7,R7               GET LENGTH OF THIS LEVEL                     
         IC    R7,ACHRLEVB                                                      
         SR    RF,RF                                                            
         IC    RF,ACHRLEVA                                                      
         SR    R7,RF                                                            
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,1,5(R2)          ANYTHING ENTERED ?                           
         BZ    AC020               NO                                           
         CLI   CHGOFFH+5,0         IF DEPARTMENT ENTERED                        
         BE    MISSIT               OFFICE MUST ALSO BE THERE                   
         CR    R5,R7               YES, CHECK LENGTH                            
         BH    LONGERR                                                          
*                                                                               
         BCTR  R7,0                                                             
         EX    R7,MVCDATA          MOVE TO 1R FOR HIERARCHY LENGTH              
         LA    R7,1(0,R7)          BUMP R7 BACK UP                              
         MVC   PCHKDEP,8(R2)       MOVE TO X'2A' RECORD ALSO                    
*                                                                               
         GOTO1 READ                READ RECORD AND                              
         GOTO1 NAMOUT               GET THE NAME                                
         EJECT                                                                  
***********************************************************************         
* VERIFY THE SUB-DEPARTMENT AND GET THE NAME.                         *         
***********************************************************************         
*                                                                               
AC020    LA    R6,0(R7,R6)         NEXT POSITION IN 1R RECORD                   
         OI    4(R2),X'20'         DEPARTMENT IS VALIDATED                      
         LA    R2,CHGSDH                                                        
         OC    CHGSD,SPACES        FILL SUB-DEPT WITH BLANKS                    
         TM    4(R2),X'20'         IF NOT PREVIOUSLY VALIDATED                  
         BO    *+8                  CLEAR THE NAME FIELDS                       
         BAS   RE,INTFLD2                                                       
*                                                                               
         SR    R7,R7                                                            
         IC    R7,ACHRLEVC         GET LENGTH OF LEVEL                          
         SR    RF,RF                                                            
         IC    RF,ACHRLEVB                                                      
         SR    R7,RF                                                            
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,1,5(R2)          OK IF NOTHING ENTERED                        
         BZ    AC030                                                            
*                                                                               
         LA    R2,CHGDEPH                                                       
         CLI   CHGDEPH+5,0         IF SUB-DEPT ENTERED                          
         BE    MISSIT               DEPT MUST ALSO BE THERE                     
         LA    R2,CHGSDH                                                        
*                                                                               
         CR    R5,R7               CHECK LENGTH                                 
         BH    LONGERR                                                          
         BCTR  R7,0                                                             
         EX    R7,MVCDATA          MOVE TO 1R FOR HIERARCHY LENGTH              
         LA    R7,1(0,R7)          BUMP R7 BACK UP                              
         MVC   PCHKSUB,8(R2)       MOVE TO X'2A' RECORD ALSO                    
*                                                                               
         GOTO1 READ                NO, READ THE RECORD                          
         GOTO1 NAMOUT               AND GET THE NAME                            
         EJECT                                                                  
***********************************************************************         
* VERIFY THE STAFF AND GET THE NAME.                                  *         
***********************************************************************         
*                                                                               
AC030    LA    R6,0(R7,R6)         NEXT POSITION IN 1R RECORD                   
         OI    4(R2),X'20'         SUB-DEPARTMENT VALIDATED                     
         LA    R2,CHGSTAH                                                       
         OC    CHGSTA,SPACES       FILL STAFF WITH BLANKS                       
         TM    4(R2),X'20'         IF NOT PREVIOUSLY VALIDATED                  
         BO    *+8                  CLEAR NAME FIELDS                           
         BAS   RE,INTFLD3                                                       
*                                                                               
         SR    R7,R7               GET LENGTH OF LEVEL                          
         IC    R7,ACHRLEVD                                                      
         SR    RF,RF                                                            
         IC    RF,ACHRLEVC                                                      
         SR    R7,RF                                                            
*                                                                               
         CLI   5(R2),0             OK IF NOTHING ENTERED                        
         BE    AC040                                                            
         LA    R2,CHGSDH                                                        
         CLI   5(R2),0             IF STAFF ENTERED                             
         BE    MISSIT               SUB-DEPT MUST ALSO BE THERE                 
         LA    R2,CHGSTAH                                                       
         ZIC   R5,5(R2)             AND MOVE SUB-DEPT TO KEY                    
         CR    R5,R7               CHECK LENGTH                                 
         BH    LONGERR                                                          
*                                                                               
         BCTR  R7,0                                                             
         EX    R7,MVCDATA          MOVE TO 1R FOR HIERARCHY LENGTH              
         MVC   PCHKSTF,8(R2)       MOVE TO X'2A' RECORD ALSO                    
*                                                                               
         GOTO1 READ                READ RECORD                                  
         GOTO1 NAMOUT               AND GET NAME                                
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VERIFY THE TASK AND GET THE NAME.                                   *         
***********************************************************************         
*                                                                               
AC040    OI    4(R2),X'20'         STAFF IS VALIDATED                           
         LA    R2,CHGTASKH                                                      
         OC    CHGTASK,SPACES      MAKE FILL CHARACTER BLANKS                   
         TM    4(R2),X'20'         IF NOT PREVIOUSLY VALIDATED                  
         BO    *+8                  CLEAR NAME FIELDS                           
         BAS   RE,INTFLD4                                                       
*                                                                               
         CLI   5(R2),0             OK IF NOTHING ENTERED                        
         BE    AC060                                                            
         MVC   KEY,SPACES          SET THIS KEY TO BLANKS                       
         MVI   KEY,X'0A'            MOVE IN TASK RECORD I.D.                    
         MVC   KEY+1(1),COMPANY     COMPANY                                     
         MVC   KEY+2(2),=C'SJ'      UNIT/LEDGER                                 
         MVC   KEY+4(2),8(R2)       AND TASK                                    
         MVC   PCHKTSK,8(R2)       PUT TASK IN CHARGE RECORD KEY                
         GOTO1 READ                READ THE RECORD                              
         MVI   ELCODE,ACANELQ       AND GET NAME ELEMENT                        
         BAS   RE,GETELIO                                                       
         USING ACANALD,R4                                                       
         MVC   CHGTNAM,ACANDESC     MOVE IN THE NAME                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VERIFY OFFICE, IF ENTERED, AND GET NAME.                            *         
***********************************************************************         
*                                                                               
AC060    OI    CHGTASKH+4,X'20'    INDICATE TASK VALIDATED                      
         MVC   KEY,SPACES                                                       
         OC    CHGCOF,SPACES       FILL OFFICE WITH BLANKS                      
         LA    R2,CHGCOFH                                                       
         TM    4(R2),X'20'         IF NOT PREVIOUSLY VALIDATED                  
         BO    *+8                  CLEAR NAME FIELDS                           
         BAS   RE,INTFLD5                                                       
         CLI   5(R2),0             OK IF NOTHING ENTERED                        
         BE    AC070                                                            
*                                                                               
AC062    TM    COMPSTA4,X'01'      TEST NEW OFFICES                             
         BO    AC064               YES                                          
         CLI   5(R2),1                                                          
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     XIT                                                              
         MVC   KEY(1),COMPANY      SET UP KEY WITH COMPANY                      
         MVC   KEY+1(2),=C'2D'      UNIT/LEDGER                                 
         MVC   KEY+3(1),8(R2)       AND OFFICE                                  
         B     AC066                                                            
*                                                                               
AC064    XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING ACOGKEY,RE                                                       
         MVI   ACOGRTYP,ACOGEQU    BUILD PRODUCTION OFFICE RECORD               
         MVI   ACOGSREC,ACOGOFF                                                 
         MVC   ACOGCUL(1),COMPANY                                               
         MVC   ACOGCUL+1(2),=C'SJ'                                              
         MVC   ACOGOFC,8(R2)                                                    
         DROP  RE                                                               
*                                                                               
AC066    MVC   PCHKOFF,8(R2)       PUT OFFICE IN CHARGE RECORD KEY ALSO         
         GOTO1 READ                 READ THE OFFICE RECORD                      
         GOTO1 NAMOUT               AND GET THE NAME                            
         EJECT                                                                  
***********************************************************************         
* VERIFY CLIENT, PRODUCT AND/OR MEDIA AND PRINT NAMES                 *         
***********************************************************************         
*                                                                               
AC070    OI    CHGCOFH+4,X'20'     INDICATE OFFICE VALIDATED                    
         OC    CHGCLI,SPACES       FILL CLIENT WITH BLANKS                      
         LA    R2,CHGCLIH                                                       
         TM    4(R2),X'20'         IF NOT PREVIOUSLY VALIDATED                  
         BO    *+8                 CLEAR NAME FIELDS                            
         BAS   RE,INTFLD6                                                       
         CLI   5(R2),0             OK IF NOTHING ENTERED                        
         BE    AC078                                                            
*                                                                               
         MVC   KEY,SPACES          CLEAR THE KEY                                
         MVC   KEY(1),COMPANY       MOVE IN COMPANY                             
         MVC   KEY+1(2),=C'SJ'      AND UNIT AND LEDGER                         
         LA    R6,KEY+3            MOVE CLIENT TO KEY                           
         ZIC   R5,5(R2)                                                         
         BCTR  R5,0                                                             
         EX    R5,MVCDATA                                                       
         MVC   PCHKCLI,8(R2)        AND CHARGE KEY ALSO                         
         GOTO1 READ                READ THE RECORD                              
         GOTO1 NAMOUT               AND GET THE NAME                            
         LR    R5,R6              SAVE CURRENT ADDRESS                          
         LA    R6,IO                                                            
         AH    R6,DATADISP                                                      
*                                                                               
AC072    CLI   0(R6),0            FIND THE X'24' ELEMENT                        
         BE    AC074                                                            
         CLI   0(R6),ACPRELQ                                                    
         BE    AC076                                                            
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     AC072                                                            
*                                                                               
AC074    MVI   ERROR,X'FE'        ERROR, OFFICE INCORRECT                       
         MVC   LOGHEAD(31),=CL31'** ERROR ** INVALID OFFICE CODE'               
         OI    6(R2),X'40'                                                      
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
*                                                                               
         USING ACPROFD,R6                                                       
AC076    MVC   PCHKOFF,ACPROFFC   MOVE OFFICE TO CHARGE RECORD                  
         LR    R6,R5               GET BACK TO WHERE WE WERE                    
         LA    R6,3(0,R6)          BUMP UP POINTERS                             
*                                                                               
*                                                                               
AC078    OI    CHGCLIH+4,X'20'     INDICATE CLIENT IS VALID                     
         OC    CHGPRO,SPACES       FILL PRODUCT WITH BLANKS                     
         LA    R2,CHGPROH                                                       
         TM    4(R2),X'20'         IF NOT PREVIOUSLY VALIDATED                  
         BO    *+8                  CLEAR NAME FIELDS                           
         BAS   RE,INTFLD7                                                       
         CLI   5(R2),0             OK IF NOT ENTERED                            
         BE    AC084                                                            
*                                                                               
         LA    R2,CHGCLIH          IF PRODUCT ENTERED                           
         GOTO1 ANY                  CLIENT MUST ALSO BE THERE                   
         LA    R2,CHGPROH          GET BACK TO WHERE WE WERE                    
         ZIC   R5,5(R2)            MOVE PRODUCT TO KEY                          
         BCTR  R5,0                                                             
         EX    R5,MVCDATA                                                       
         MVC   PCHKPRO,8(R2)        AND CHARGE KEY                              
         GOTO1 READ                READ THE RECORD                              
         GOTO1 NAMOUT               AND GET THE NAME                            
         LR    R5,R6              SAVE CURRENT ADDRESS                          
         LA    R6,IO                                                            
         AH    R6,DATADISP                                                      
*                                                                               
AC080    CLI   0(R6),0            FIND THE X'24' ELEMENT                        
         BE    AC084                                                            
         CLI   0(R6),ACPRELQ                                                    
         BE    AC082                                                            
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     AC080                                                            
*                                                                               
         USING ACPROFD,R6                                                       
AC082    CLC   ACPROFFC,SPACES                                                  
         BNH   *+10                                                             
         MVC   PCHKOFF,ACPROFFC   MOVE OFFICE TO CHARGE RECORD                  
*                                                                               
AC084    OI    CHGPROH+4,X'20'     INDICATE PRODUCT IS VALID                    
         CLC   PCHKOFF,SPACES      ANY OFFICE?                                  
         BNH   AC092               NO                                           
         LA    R2,CHGCOFH                                                       
         CLC   CHGCOF,PCHKOFF     OK IF THEY MATCH                              
         BE    AC086                                                            
         CLI   5(R2),0            IF OFFICE ENTERED, THERE IS AN                
         BNE   AC074               ERROR                                        
*                                                                               
AC086    MVC   SAVEKEY,KEY         HOLD KEY SO FAR                              
         TM    COMPSTA4,X'01'      TEST NEW OFFICES                             
         BO    AC088               YES                                          
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'2D'                                                  
         MVC   KEY+3(1),PCHKOFF                                                 
         B     AC090                                                            
*                                                                               
AC088    XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING ACOGKEY,RE                                                       
         MVI   ACOGRTYP,ACOGEQU    BUILD PRODUCTION OFFICE RECORD               
         MVI   ACOGSREC,ACOGOFF                                                 
         MVC   ACOGCUL(1),COMPANY                                               
         MVC   ACOGCUL+1(2),=C'SJ'                                              
         MVC   ACOGOFC,PCHKOFF                                                  
         DROP  RE                                                               
*                                                                               
AC090    MVC   CHGCONM,SPACES      CLEAR OFFICE NAME                            
         OI    CHGCONMH+6,X'80'     TRANSMIT OFFICE NAME                        
         OI    CHGCOFH+6,X'80'      AND CODE                                    
         MVC   CHGCOF,PCHKOFF      PUT OFFICE ON SCREEN                         
         GOTO1 READ                 READ THE OFFICE RECORD                      
         GOTO1 NAMOUT               AND GET THE NAME                            
         MVC   KEY,SAVEKEY                                                      
*                                                                               
AC092    MVC   KEY,CHGKEY          SETUP KEY                                    
         LA    R2,CHGOFFH          SET UP CURSOR FOR ERROR                      
         CLI   LOGACT,C'N'         TEST IF ADDING NEW RECORD                    
         BNE   XIT                 NO                                           
*                                                                               
         XC    IO2(42),IO2                                                      
         BAS   RE,SETUP                                                         
         GOTO1 HIGH                SEE IF RECORD EXISTS                         
         BAS   RE,RESET                                                         
         NI    4(R2),X'DF'         RESET VALIDATION BIT                         
         CLC   KEYSAVE,KEY         EXIT IF IT DOESN'T EXIST                     
         BNE   XIT                                                              
*                                                                               
         MVI   ERROR,RECONFLE      SET ERROR CODE IF IT DOES                    
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* DISPLAY A RECORD.                                                  *          
**********************************************************************          
*                                                                               
AC200    DS    0H                                                               
         CLI   MODE,DSPLYREC       IF NOT DISPLAY, SEE WHAT TO DO               
         BNE   AC300                                                            
         BAS   RE,ACDSPLY                                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD A RECORD FOR NEW/CHANGE.                                      *         
***********************************************************************         
*                                                                               
AC300    CLI   MODE,BUILDREC       IF NOT BUILD A RECORD, GET OUT               
         BNE   XIT                                                              
         CLI   LOGACT,C'N'         BYPASS DELETE IF THIS IS 'NEW'               
         BE    AC310                OR NOT DELETE                               
         MVC   KEY,CHGKEY                                                       
         XC    IO2(42),IO2         SETUP AND GET RECORD                         
         MVI   UPDATE,C'Y'          FOR UPDATE                                  
         LA    R2,CHGDEPH                                                       
         BAS   RE,SETUP                                                         
         GOTO1 READ                                                             
         BAS   RE,RESET                                                         
*                                                                               
         LA    R6,IO                                                            
         LA    R2,CHGDAT1H                                                      
         CLC   8(6,R2),=C'DELETE'                                               
         BNE   AC305                                                            
*                                                                               
         USING ACKEYD,R6                                                        
         OI    ACSTATUS,X'80'      FLAG RECORD FOR DELETE                       
         LA    R4,IO2                                                           
         LH    R5,ACLENGTH                                                      
         LR    R7,R5                                                            
         MVCL  R4,R6               MOVE FROM IO TO IO2                          
         B     AC370                                                            
*                                                                               
AC305    LA    R4,IO2                                                           
         LH    R5,ACLENGTH                                                      
         LR    R7,R5                                                            
         MVCL  R4,R6               MOVE FROM IO TO IO2                          
         GOTO1 REMANEL,DMCB,(X'53',0)                                           
         B     AC315                                                            
*                                                                               
AC310    LA    R4,IO2              CLEAR RECORD IN IO2                          
         LA    R5,IOLENQ                                                        
         LA    R6,*                                                             
         XR    R7,R7                                                            
         MVCL  R4,R6                                                            
         LA    R6,IO2                                                           
         XC    IO2(42),IO2                                                      
         MVC   KEY,CHGKEY         BUILD NEW RECORD                              
         MVC   ACKEYACC(32),KEY                                                 
         MVC   ACLENGTH,DATADISP                                                
         GOTO1 STATIN                                                           
         DROP  R6                                                               
*                                                                               
AC315    LA    R5,CHGDAT1H         POINT TO FIRST FIELD                         
         USING LND,R5                                                           
AC320    LA    R4,ELEMENT          GET ADDRESS OF WORK AREA                     
         USING ACCHARGD,R4                                                      
         XC    ELEMENT,ELEMENT     CLEAR THE ELEMENT                            
         MVI   ACCHEL,X'53'        MOVE IN ELEMENT CODE                         
         MVI   ACCHLEN,X'13'        AND THE LENGTH                              
         LA    R2,LNDRATH          GET FIRST FIELD                              
         GOTO1 ANY                 EXIT IF NOTHING THERE                        
         GOTO1 VALICASH                                                         
         ZAP   DUB,DUB             NEGATIVE NOT ALLOWED                         
         BM    AC330                                                            
         CP    DUB,=P'999999'      MAXIMUM IS 9,999.99                          
         BC    12,AC340            TOO HIGH - ERROR                             
*                                                                               
AC330    MVI   ERROR,25            INDICATE ERROR                               
         B     XIT                  AND EXIT                                    
*                                                                               
AC340    CLI   LNDSEQ,X'00'                                                     
         BNE   AC345                                                            
         L     R3,LASTNUM                                                       
         LA    R3,1(R3)                                                         
         STC   R3,LNDSEQ                                                        
         ST    R3,LASTNUM                                                       
*                                                                               
AC345    MVC   ACCHCM(1),LNDSEQ    GET SEQUENCE NUMBER                          
         ZAP   ACCHAMT,DUB         PUT IT INTO RECORD                           
         LA    R2,LNDDATH                                                       
         CLI   5(R2),0             ADD ELEMENT AND GET NEXT ENTRY               
         BE    AC350                IF NO DATE                                  
         GOTO1 DATVAL,DMCB,(0,LNDDAT),WORK                                      
         OC    DMCB,DMCB           INDICATE ERROR AND EXIT IF                   
         BNZ   *+12                INVALID                                      
         MVI   ERROR,DATERR                                                     
         B     XIT                                                              
*                                                                               
**********************************************************************          
* CHECK FOR DUPLICATE DATES                                          *          
**********************************************************************          
         GOTO1 DATCON,DMCB,(0,WORK),(1,ACCHDTE)    UPDATE RECORD                
*                                                                               
AC350    DS    0H                                                               
         GOTO1 DATCON,DMCB,(1,ACCHDTE),(8,MYDATE)   CONVERT DATE                
         LA    R7,CHGDAT1H         POINT TO FIRST DATE                          
*                                                                               
AC355    DS    0H                                                               
         CLI   21(R7),0            IS THERE A RATE?                             
         BE    AC358               NO                                           
         CR    R7,R5               CURRENT DATE?                                
         BE    AC358               YES                                          
         CLC   MYDATE,8(R7)        DUPLICATE DATE?                              
         BNE   *+12                NO                                           
*                                                                               
AC356    MVI   ERROR,DUPLCATE                                                   
         B     XIT                                                              
         LR    R3,R2                                                            
         LR    R2,R7                                                            
         GOTO1 DATVAL,DMCB,(0,8(R7)),WORK                                       
         CLI   5(R7),0                                                          
         BE    AC357                                                            
         OC    DMCB,DMCB           INDICATE ERROR AND EXIT IF                   
         BNZ   *+12                INVALID                                      
         MVI   ERROR,DATERR                                                     
         B     XIT                                                              
*                                                                               
AC357    DS    0H                                                               
         LR    R2,R3               RESTORE                                      
         GOTO1 DATCON,DMCB,(0,WORK),(8,MYDATE2)                                 
         CLC   MYDATE,MYDATE2      DUPLICATE DATE?                              
         BE    AC356               YES                                          
*                                                                               
AC358    LA    R7,LNDLNG(R7)       GO TO NEXT DATE                              
         CLC   CHGENDH,0(R7)       END OF DATES?                                
         BNE   AC355                                                            
*                                                                               
         USING COMFACSD,R3                                                      
         L     R3,COMFACS                                                       
         GOTO1 CHELLO,DMCB,(C'P',=C'ACCOUNT '),IO2,ELEMENT                      
         CLI   DMCB+12,0                                                        
         BE    AC360                                                            
         DC    H'0'                                                             
*                                                                               
AC360    LA    R5,LNDLNG(R5)        AND LOOK AT NEXT                            
         CLI   0(R5),9             IF NOT MORE DATA, ADD RECORD                 
         BE    AC370                                                            
         CLI   LNDDATH+5,0         GO HANDLE DATE                               
         BNE   AC320                                                            
         CLI   LNDRATH+5,0         OR RATE                                      
         BNE   AC320                                                            
         B     AC360               OR TRY NEXT ENTRY                            
         DROP  R5                                                               
*                                                                               
AC370    OI    DMINBTS,X'88'       LOOK AT DELETED RECORDS                      
         MVC   KEY,IO2                                                          
         BAS   RE,SETUP            SETUP AND READ RECORD                        
         GOTO1 HIGH                                                             
         BAS   RE,RESET                                                         
         NI    DMINBTS,X'FF'-X'88'                                              
         CLC   KEYSAVE,KEY         IF NOT THERE, ADD IT                         
         BNE   AC380                                                            
         BAS   RE,SETUP1                                                        
         GOTO1 UPREC,1             PUT RECORD                                   
         BAS   RE,RESET                                                         
         MVI   ERROR,X'FF'                                                      
         B     AC400                                                            
*                                                                               
AC380    BAS   RE,SETUP1                                                        
         GOTO1 UPREC,0             ADD RECORD                                   
         BAS   RE,RESET                                                         
*                                                                               
AC400    BAS   RE,ACDSPLY                                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY DATA                                             *         
***********************************************************************         
*                                                                               
ACDSPLY  NTR1                                                                   
         TWAXC CHGDAT1H,CHGENDH    CLEAR WORK AREA                              
         LA    R2,CHGOFFH                                                       
         MVC   KEY,CHGKEY          SETUP AND GET RECORD                         
         BAS   RE,SETUP                                                         
         GOTO1 READ                                                             
         BAS   RE,RESET                                                         
*                                                                               
         USING ACCHARGD,R4                                                      
XSORTEL  SR    R5,R5                                                            
         MVI   ELCODE,X'53'                                                     
         BAS   RE,GETELIO                                                       
         BNE   XIT                                                              
         ST    R4,XSORTADD                                                      
         B     XBUMP                                                            
*                                                                               
XGETNXT  BAS   RE,NEXTEL                                                        
         BNE   QSORTIT                                                          
*                                                                               
XBUMP    LA    R5,1(R5)                                                         
         CLI   ACCHCM,X'00'        DO WE HAVE A SEQUENCE NUMBER ?               
         BE    SETR5               NO, SET ONE                                  
         CLI   ACCHCM,X'40'        YES, SURE IT'S A NUMBER ?                    
         BL    *+8                 YES                                          
*                                                                               
SETR5    STC   R5,ACCHCM                                                        
         B     XGETNXT                                                          
*                                                                               
QSORTIT  ST    R5,LASTNUM          SAVE NUMBER FOR NEXT RECORD                  
         L     R2,XSORTADD                                                      
         GOTO1 CALLOV,DMCB,0,X'D9000A50'                                        
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R2),(R5),19,4,12                                      
         LA    R2,CHGDAT1H         POSITION CURSOR FOR AMEND                    
         LA    R5,CHGDAT1H         ADDRESS FIRST FIELD                          
         USING LND,R5                                                           
         LA    R4,IO               GET SAVED CHARGE RECORD                      
         AH    R4,DATADISP         ADD DISPLACEMENT OF ELEMENT                  
*                                                                               
AC210    CLI   0(R4),X'53'         LOOK FOR X'53' ELEMENT OR                    
         BE    AC230                END-OF-RECORD                               
         CLI   0(R4),0                                                          
         BE    XIT                                                              
*                                                                               
AC220    ZIC   R0,1(R4)            NOT RIGHT ONE, BUMP ADDRESS                  
         AR    R4,R0                AND KEEP LOOKING                            
         B     AC210                                                            
*                                                                               
AC230    EDIT  ACCHAMT,(7,LNDRAT),2,ALIGN=LEFT,FLOAT=-                          
         MVC   LNDSEQ,ACCHCM                                                    
         OC    ACCHDTE,ACCHDTE     IF NO EFFECTIVE DATE, GET                    
         BZ    AC240                THE NEXT ENTRY                              
         GOTO1 DATCON,DMCB,(1,ACCHDTE),(8,LNDDAT)                               
*                                                                               
AC240    LA    R5,LNDLNG(R5)       DONE, GET NEXT ENTRY AND                     
         B     AC220                ELEMENT                                     
         DROP  R5                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE NAME FIELDS AND FIELD HEADERS                            *         
***********************************************************************         
*                                                                               
INTFLD0  MVC   CHGONAM,SPACES      CLEAR OFFICE NAME                            
         OI    CHGONAMH+6,X'80'     TRANSMIT                                    
         OI    6(R2),X'80'          RETRANSMIT CODE ALSO                        
         NI    CHGDEPH+4,X'DF'      TURN OFF DEPT BIT                           
*                                                                               
INTFLD1  MVC   CHGDNAM,SPACES      CLEAR DEPT NAME                              
         OI    CHGDNAMH+6,X'80'     TRANSMIT                                    
         OI    6(R2),X'80'          RETRANSMIT CODE ALSO                        
         NI    CHGSDH+4,X'DF'       TURN OFF SUB-DEPT BIT                       
*                                                                               
INTFLD2  MVC   CHGSNAM,SPACES      CLEAR SUB-DEPT NAME                          
         OI    CHGSNAMH+6,X'80'     TRANSMIT                                    
         OI    6(R2),X'80'          RETRANSMIT CODE ALSO                        
         NI    CHGSTAH+4,X'DF'      TURN OFF STAFF BIT                          
*                                                                               
INTFLD3  MVC   CHGSTNO,SPACES      CLEAR STAFF NAME                             
         OI    CHGSTNOH+6,X'80'     TRANSMIT                                    
         OI    6(R2),X'80'          RETRANSMIT CODE ALSO                        
         NI    CHGTASKH+4,X'FD'     TURN OFF TASK BIT                           
*                                                                               
INTFLD4  MVC   CHGTNAM,SPACES      CLEAR TASK NAME                              
         OI    CHGTNAMH+6,X'80'     INDICATE TRANSMIT FIELD                     
         OI    6(R2),X'80'          RETRANSMIT CODE ALSO                        
         NI    CHGCOFH+4,X'DF'      TURN OFF OFFICE BIT                         
*                                                                               
INTFLD5  MVC   CHGCONM,SPACES      CLEAR OFFICE NAME                            
         OI    CHGCONMH+6,X'80'     TRANSMIT                                    
         OI    6(R2),X'80'          RETRANSMIT CODE ALSO                        
         NI    CHGCLIH+4,X'DF'      TURN OFF CLIENT BIT                         
*                                                                               
INTFLD6  MVC   CHGCNAM,SPACES      CLEAR CLIENT NAME                            
         OI    CHGCNAMH+6,X'80'     TRANSMIT                                    
         OI    6(R2),X'80'          RETRANSMIT CODE ALSO                        
         NI    CHGPROH+4,X'DF'      TURN OFF PRODUCT BIT                        
*                                                                               
INTFLD7  MVC   CHGPNAM,SPACES      CLEAR PRODUCT NAME                           
         OI    CHGPNAMH+6,X'80'     TRANSMIT                                    
         OI    6(R2),X'80'          RETRANSMIT CODE ALSO                        
         MVI   ANYKEY,C'Y'                                                      
         BR    RE                                                               
         EJECT                                                                  
*********************************************************************           
* ROUTINE TO HANDLE RECORD UPDATES                                  *           
* AT ENTRY, R1=0 FOR ADD, R1=1 FOR PUT                              *           
*********************************************************************           
UPREC    NTR1  WORK=(R5,RAPPERX-RAPPERD)                                        
         USING RAPPERD,R5                                                       
         STC   R1,RAADDSW          SAVE ADD/PUT FLAG                            
         LA    RF,IO2                                                           
*                                                                               
         CLI   RAPTR,C'Y'          TEST FOR RECORD ACTIVITY POINTERS            
         BNE   UPREC10             NO                                           
*                                                                               
         XC    RAPBLK(RAPBLKL),RAPBLK                                           
         MVI   RAPACTN,RAPAELEM                                                 
         MVC   RAPCPY,COMPANY                                                   
         MVI   RAPRTYP,RAPKRRAT    RECORD TYPE RATE MAINTENANCE                 
         MVI   RAPEMU,C'Y'                                                      
         MVC   RAPACOM,COMFACS                                                  
         LA    RE,IO2                                                           
         ST    RE,RAPAREC                                                       
         GOTO1 RAPPER,RAPBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPREC10  L     RF,ADDREC                                                        
         CLI   RAADDSW,0           TEST TO ADD RECORD                           
         BE    *+8                 YES                                          
         L     RF,PUTREC                                                        
*                                                                               
         BASR  RE,RF                                                            
*                                                                               
         CLI   RAPTR,C'Y'          DID WE GENERATE RAPPER PNTRS                 
         BNE   UPRECX                                                           
*                                                                               
         MVI   RAPACTN,RAPAPTR                                                  
         GOTO1 RAPPER,RAPBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPRECX   B     XIT                                                              
         DROP  R5                                                               
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
SETUP1   OI    RECOREL,FULLKEY                                                  
         BR    RE                                                               
*                                                                               
RESET    NI    RECOREL,X'FF'-FULLKEY                                            
         BR    RE                                                               
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMDBD                                                       
CHGKEY   DS    CL32                                                             
LASTNUM  DS    X                                                                
         EJECT                                                                  
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACLFMEQU                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACLFMEQU                                                       
         PRINT ON                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
*                                                                               
* DSECT FOR ACRAPPER BLOCK                                                      
*                                                                               
RAPPERD  DSECT                                                                  
RAADDSW  DS    XL1                 0=ADD, 1=PUT                                 
         DS    0F                                                               
       ++INCLUDE ACRAPPERD                                                      
RAPPERX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
       ++INCLUDE ACLFMWORK                                                      
         EJECT                                                                  
CHARGED  DSECT                                                                  
ELCODE   DS    CL1                                                              
FULLKEY  EQU   X'08'                                                            
SAVEKEY  DS    CL32                                                             
XSORTADD DS    F                                                                
SAVELED  DS    CL(ACHRLENQ)                                                     
MYDATE   DS    CL8                                                              
MYDATE2  DS    CL8                                                              
*                                                                               
LND      DSECT                                                                  
LNDDATH  DS    CL8                 DATE HEADER                                  
LNDDAT   DS    CL8                 DATE FIELD                                   
LNDRATH  DS    CL8                 RATE HEADER                                  
LNDRAT   DS    CL7                 RATE FIELD                                   
LNDSEQH  DS    CL8                 SEQUENCE HEADER                              
LNDSEQ   DS    CL1                 SEQUENCE FIELD                               
LNDLNG   EQU   *-LND                                                            
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'085ACLFM28   01/21/04'                                      
         END                                                                    
