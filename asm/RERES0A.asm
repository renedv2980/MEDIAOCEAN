*          DATA SET RERES0A    AT LEVEL 062 AS OF 05/01/02                      
*PHASE T8190AA,*                                                                
         TITLE 'T8190A - RESEARCH DAYPART RECORD - HISTORY'                     
**********************************************************************          
*                                                                    *          
*        RESFM03 (T8190A) --- RESEARCH DAYPART RECORD                *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* 08OCT90  (EFJ) --- TOMBSTONE ADDED, PHASE CARD CHANGED TO 'A'      *          
*                                                                    *          
*   OCT95  (BOB) --- CLONED FROM SFM, PHASE CARD CHANGED TO 'A'      *          
*                                                                    *          
**********************************************************************          
         TITLE 'T8190A - RESEARCH DAYPART RECORD - INIT'                        
**********************************************************************          
*                                                                    *          
*        INITIALIZATION                                              *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
T8190A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**190A**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         TITLE 'T8190A - RESEARCH DAYPART RECORD - CHKMODE'                     
**********************************************************************          
*                                                                    *          
*        CHECK ON MODE CALL                                          *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
CHKMODE  DS    0H                                                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    DREC                DISPLAY ONLY HERE                            
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT LIST RECORDS                           
         BE    LIST                                                             
*                                                                               
XIT      XIT1                                                                   
         TITLE 'T8190A - RESEARCH DAYPART RECORD - VKEY'                        
**********************************************************************          
*                                                                    *          
*        VALIDATE KEY - CONSISTS OF 1 BYTE DAYPART CODE              *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
VKEY     LA    R2,DPTDTCH          POINT TO DAYPART CODE FIELD                  
         XC    SVDPT,SVDPT         INIT DPT CODE SAVEAREA                       
*                                                                               
*        VALIDATE REP                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
*                                                                               
         USING RREPKEY,R6                                                       
         MVI   RREPKTYP,X'01'      REP RECORD                                   
         MVC   RREPKREP,AGENCY                                                  
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RREPKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'01'                                                     
*                                                                               
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
*                                                                               
         USING RREPELEM,R6                                                      
*                                                                               
         MVC   CPAREP,RREPPAR      SAVE PARENT REP CODE                         
*                                                                               
         DROP  R6                                                               
*                                                                               
*        RETURN RESEARCH DAYPART RECORD KEY FOR GENCON                          
*                                                                               
         LA    R4,KEY              INIT RESEARCH RECORD DAYPART KEY             
         XC    KEY,KEY                                                          
         USING RRDPKEY,R4                                                       
*                                                                               
         MVI   RRDPKTYP,RRDPKIDQ   SET RECORD TYPE ID                           
         MVC   RRDPKREP,CPAREP     SET REP ID                                   
*                                                                               
         LA    R2,DPTDTCH          VALIDATE RESEARCH DAYPART                    
*                                                                               
         CLI   ACTNUM,ACTLIST      IF ACTION LIST                               
         BE    *+8                                                              
         CLI   ACTNUM,ACTREP       OR ACTION REPORT, THEN                       
         BNE   VKEY10                                                           
*                                                                               
         LA    R2,LDTDTCH          POINT TO LIST DAYPART CODE FIELD             
*                                                                               
VKEY10   DS    0H                                                               
*                                                                               
         MVC   RRDPKDPT,8(R2)      ADD TO KEY                                   
         OC    RRDPKDPT,SPACES     MAKE UPPERCASE                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         MVC   SVDPT,RRDPKDPT      SAVE CODE FOR THE FUTURE                     
*                                                                               
VKEXT    B     XIT                                                              
*                                                                               
         TITLE 'T8190A - RESEARCH DAYPART RECORD - DKEY'                        
**********************************************************************          
*                                                                    *          
*        DISPLAY  KEY - CONSISTS OF 1 BYTE DAYPART CODE              *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
DKEY     DS    0H                                                               
*                                                                               
         LA    R4,KEY              ESTABLISH RETURNED KEY                       
         USING RRDPKEY,R4                                                       
*                                                                               
         LA    R2,DPTDTCH          DISPLAY KEY (FOR SELECT)                     
         MVC   8(1,R2),RRDPKDPT                                                 
         MVI   5(R2),1             FORCE TO LENGTH 1                            
         OI    6(R2),X'80'         FORCE RE-DISPLAY OF FIELD                    
*                                                                               
         B     XIT                                                              
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'T8190A - RESEARCH DAYPART RECORD - VREC'                        
**********************************************************************          
*                                                                    *          
*        VALIDATE RECORD                                             *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
VREC     DS    0H                                                               
*                                                                               
         L     R6,AIO1             ESTABLISH RECORD IN IO AREA AS               
         USING RRDPRECD,R6         RESEARCH DAYPART RECORD                      
*                                                                               
*        SHORT NAME REQUIRED                                                    
*                                                                               
         LA    R2,DPTDTSH          POINT TO SHORT NAME FIELD                    
*                                                                               
         CLI   5(R2),0             INPUT REQUIRED                               
         BE    VRDPTNOE                                                         
*                                                                               
         CLI   5(R2),L'RRDPSNAM    CHECK MAX ALLOWED INPUT                      
         BH    VRDPTNVE                                                         
*                                                                               
         GOTO1 ANY                 READ IN SHORT NAME                           
*                                                                               
         MVI   RRDPCODE,X'01'      SET DPT ELEMENT CODE                         
         MVI   RRDPELLN,RRDPELML   SET DPT ELEMENT LENGTH                       
*                                                                               
         MVC   RRDPSNAM,WORK       SAVE SHORT NAME                              
*                                                                               
*        LONG NAME REQUIRED                                                     
*                                                                               
         LA    R2,DPTDTLH          POINT TO LONG NAME FIELD                     
*                                                                               
         CLI   5(R2),L'RRDPLNAM    CHECK MAX ALLOWED INPUT                      
         BH    VRDPTNVE                                                         
*                                                                               
         GOTO1 ANY                 READ IN LONG NAME                            
*                                                                               
         MVC   RRDPLNAM,WORK       SAVE LONG NAME                               
*                                                                               
VRECX    DS    0H                                                               
*                                                                               
         B     DREC                                                             
*                                                                               
VRDPTNOE MVI   ERROR,MISSING       ENTRY REQUIRED                               
         B     TRAPERR                                                          
VRDPTNVE MVI   ERROR,INVALID       ENTRY INVALID                                
         B     TRAPERR                                                          
*                                                                               
         DROP  R6                                                               
*                                                                               
         TITLE 'T8190A - RESEARCH DAYPART RECORD - DREC'                        
**********************************************************************          
*                                                                    *          
*        DISPLAY  RECORD                                             *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
DREC     EQU   *                                                                
*                                                                               
         L     R6,AIO1             ESTABLISH RECORD IN IO AREA                  
         USING RRDPRECD,R6         AS RESEARCH DAYPART RECORD                   
*                                                                               
         LA    R2,DPTDTSH          POINT TO DAYPART SHORT NAME FIELD            
*                                                                               
         MVC   8(L'RRDPSNAM,R2),RRDPSNAM  DISPLAY SHORT NAME                    
*                                                                               
         MVI   5(R2),L'RRDPSNAM    SET FIELD LENGTH                             
         OI    6(R2),X'80'         FORCE FIELD TRANSMISSION                     
*                                                                               
         LA    R2,DPTDTLH          POINT TO DAYPART LONG NAME FIELD             
*                                                                               
         MVC   8(L'RRDPLNAM,R2),RRDPLNAM  DISPLAY LONG  NAME                    
*                                                                               
         MVI   5(R2),L'RRDPLNAM    SET FIELD LENGTH                             
         OI    6(R2),X'80'         FORCE FIELD TRANSMISSION                     
*                                                                               
DRECX    B     XIT                                                              
*                                                                               
         DROP  R6                                                               
*                                                                               
         TITLE 'T8190A - RESEARCH DAYPART RECORD - LIST'                        
**********************************************************************          
*                                                                    *          
*        LIST RESEARCH DAYPARTS                                      *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
LIST     EQU   *                                                                
*                                                                               
         LA    R2,LDTSELH                                                       
*                                                                               
         MVI   NLISTS,14           NUMBER OF LIST LINES ON SCREEN               
*                                                                               
         BAS   RE,CLRSCRN                                                       
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LS100                                                            
*                                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
LS100    LA    R4,KEY                                                           
         OC    KEY,KEY             WAS A KEY PROVIDED?                          
         BNZ   LS220                                                            
         USING RRDPKEY,R4                                                       
         MVI   RRDPKTYP,RRDPKIDQ                                                
         MVC   RRDPKREP,CPAREP     REP                                          
         CLI   MODE,PRINTREP                                                    
         BE    *+10                NO FILTERING FOR REPORT                      
         MVC   RRDPKDPT,SVDPT                                                   
*                                                                               
         GOTO1 HIGH                                                             
         B     LS220                                                            
         DROP  R4                                                               
*                                                                               
LS200    GOTO1 SEQ                                                              
LS220    CLC   KEY(26),KEYSAVE     CHECKMAIN C/B                                
         BNE   XIT                                                              
         GOTO1 GETREC                                                           
         MVC   LISTAR,SPACES       SHOW SCREEN                                  
         LA    R3,LISTAR                                                        
         USING LISTD,R3                                                         
         L     R6,AIO                                                           
         USING RRDPRECD,R6                                                      
         MVC   LISTCDE,RRDPKDPT                                                 
         MVC   LISTSNAM,RRDPSNAM                                                
         MVC   LISTLNAM,RRDPLNAM                                                
*                                                                               
LS400    CLI   MODE,PRINTREP                                                    
         BE    LS500                                                            
         SPACE                                                                  
         MVC   DMDSKADD,KEY+28                                                  
         GOTO1 LISTMON                                                          
         B     LS200                                                            
         SPACE                                                                  
LS500    MVC   P(72),LISTAR                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LS200                                                            
         DROP  R6,R3                                                            
         EJECT                                                                  
HEADING  DS    0H                                                               
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H2,1,AGYNAME                                                     
         SSPEC H1,50,C'RESEARCH DAYPART RECORDS'                                
         SSPEC H2,50,C'------------------------'                                
         SSPEC H1,93,RUN                                                        
         SSPEC H2,93,REPORT                                                     
         SSPEC H2,109,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         LA    R3,H8                                                            
         USING LISTD,R3                                                         
         MVC   LISHCDE,=C'DPT'                                                  
         MVC   LISHCDE+132,DASHS                                                
         MVC   LISHSNAM,=C'SHORT NAME'                                          
         MVC   LISHSNAM+132,DASHS                                               
         MVC   LISHLNAM,=C'LONG NAME'                                           
         MVC   LISHLNAM+132,DASHS                                               
         B     XIT                                                              
         EJECT                                                                  
CLRSCRN  NTR1                                                                   
*                                                                               
*        ROUTINE TO CLEAR THE SCREEN                                            
*        FROM FIELD AT R2                                                       
*                                                                               
         SR    RE,RE                                                            
*                                                                               
CS010    IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'02'         IF EXTENDED HEADER                           
         BNO   *+8                                                              
         SH    RE,=H'8'               SUBTRACT ITS LENGTH                       
*                                                                               
         EX    RE,CSCLC                                                         
         BE    CS020                                                            
         EX    RE,CSOC                                                          
         BZ    CS020                                                            
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS020    DS    0H                                                               
         IC    RE,0(R2)                                                         
         LA    R2,0(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS010                                                            
         B     XIT                                                              
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
TRAPERR  GOTO1 ERREX                                                            
         SPACE 3                                                                
RELO     DS    A                                                                
DASHS    DC    47C'-'                                                           
         LTORG                                                                  
         EJECT                                                                  
LISTD    DSECT                                                                  
LISTLINE DS    0X                                                               
         DS    CL5                                                              
LISTCDE  DS    CL1                                                              
         DS    CL8                                                              
LISTSNAM DS    CL3                                                              
         DS    CL7                                                              
LISTLNAM DS    CL15                                                             
         ORG   LISTLINE                                                         
         DS    CL4                                                              
LISHCDE  DS    CL3                                                              
         DS    CL4                                                              
LISHSNAM DS    CL10                                                             
         DS    CL3                                                              
LISHLNAM DS    CL9                                                              
         EJECT                                                                  
* RERESWRK                                                                      
       ++INCLUDE RERESWRK                                                       
         ORG   CONTAGH                                                          
* RERESFAD                                                                      
       ++INCLUDE RERESFAD                                                       
         ORG   CONTAGH                                                          
* RERESEAD                                                                      
       ++INCLUDE RERESEAD                                                       
         EJECT                                                                  
* REGENRDP                                                                      
       ++INCLUDE REGENRDP                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062RERES0A   05/01/02'                                      
         END                                                                    
