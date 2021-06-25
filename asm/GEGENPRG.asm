*          DATA SET GEGENPRG   AT LEVEL 021 AS OF 01/20/04                      
*PHASE T00A37A                                                                  
         TITLE 'T00A37 - PROGRAM RECORDS MAINTENANCE'                           
T00A37   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GENPRG                                                       
         LA    R9,2048(RB)         SECOND BASE                                  
         LA    R9,2048(R9)                                                      
         USING T00A37+4096,R9                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS (AND SPACES)             
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR GENCON                       
         MVI   HALF,0                                                           
         CLI   4(R1),C'F'          FLAG FOR NOT CLEARING CERTAIN FIELDS         
         BNE   *+10                                                             
         MVC   HALF(1),7(R1)       FIRST FIELD ID NUMBER TO OVERWRITE           
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,RECADD         ADD RECORD                                   
         BE    RADD                                                             
         CLI   MODE,RECPUT         PUT RECORD                                   
         BE    RPUT                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    DELR                                                             
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* ADD RECORD                                                                    
*                                                                               
RADD     LA    R4,KEY              REBUILD KEY                                  
         XC    KEY,KEY                                                          
         USING CTLKEYD,R4                                                       
         MVI   CTLKID,CTLKIDQ                                                   
         MVC   CTLAGID,TWAAGY                                                   
         MVC   CTLSYS(3),SVSYSPH                                                
         BAS   RE,OVRSYS                                                        
         L     R2,EFHTAG                                                        
         BAS   RE,BUMP                                                          
         GOTO1 ANY                                                              
         MVC   CTLNAME,WORK                                                     
         DROP  R4                                                               
*                                                                               
         L     RE,AIO              PRE-CLEAR I/O                                
         L     RF,SIZEIO                                                        
         XCEF                                                                   
         L     RE,AIO              MOVE KEY INTO I/O                            
         LH    R1,LKEY                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),KEY                                                      
*                                                                               
         B     BUILD                                                            
         SPACE 5                                                                
* PUT RECORD                                                                    
*                                                                               
RPUT     LA    R4,KEY              REBUILD KEY                                  
         XC    KEY,KEY                                                          
         USING CTLKEYD,R4                                                       
         MVI   CTLKID,CTLKIDQ                                                   
         MVC   CTLAGID,TWAAGY                                                   
         MVC   CTLSYS(3),SVSYSPH                                                
         BAS   RE,OVRSYS                                                        
         L     R2,EFHTAG                                                        
         BAS   RE,BUMP                                                          
         GOTO1 ANY                                                              
         MVC   CTLNAME,WORK                                                     
         DROP  R4                                                               
*                                                                               
         GOTO1 READ                GET RECORD BACK                              
*                                                                               
         L     R2,EFHTAG                                                        
         LA    R0,4                                                             
         BAS   RE,BUMP                                                          
         BCT   R0,*-4                                                           
* NOW POINTING AT INPUT DESCRIPTION FIELD                                       
         CLI   8(R2),C'('          TEST SECURITY FOR THIS REPORT                
         BNE   RPUT10              NO - CHECK IF THERE USED TO BE               
*                                                                               
* FIRST CHAR INDICATES SECURITY REQUIRED                                        
*                                                                               
         MVI   ELCODE,CTLPCODQ     SET TO FIND PREV SECURITY ELEM               
         L     R6,AIO                                                           
         BAS   RE,GETEL            GET SECURITY ELEM                            
         BNE   BUILD               NO PREVIOUS SECURITY - SO IGNORE             
* GET CURRENT PID (IF ANY)                                                      
         USING CTLPIDD,R6                                                       
RPUTSEC  GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'       IS PASSWORD PROTECT ACTIVE                   
         BZ    BUILD               NO - IGNORE SECURITY REQUEST                 
         CLC   FAPASSWD,CTLPID     DOES PERSON MATCH                            
         BE    BUILD                                                            
         MVI   ERROR,SECLOCK       ELSE SECURITY ERROR                          
         B     TRAPERR                                                          
         DROP  R1,R6                                                            
*                                                                               
* NO SECURITY ON THIS RECORD NOW - CHECK PREVIOUS                               
*                                                                               
RPUT10   MVI   ELCODE,CTLDCODQ     FIND PREVIOUS DESC ELEM                      
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   BUILD                                                            
*                                                                               
         CLI   2(R6),C'('          TEST FIRST CHAR                              
         BNE   BUILD                                                            
* RECORD WAS PROTECTED - OK IF PIDS MATCH                                       
         MVI   ELCODE,CTLPCODQ     FIND PREVIOUS SECURITY ELEM                  
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    RPUTSEC             GO SEE IF PIDS MATCH                         
         B     BUILD               IF NO PREV SEC ELEM, IGNORE                  
         EJECT                                                                  
* BUILD RECORD                                                                  
*                                                                               
BUILD    DS    0H                                                               
         MVI   ELCODE,CTLOCODQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         L     R2,EFHOUT                                                        
         CLI   8(R2),C'@'          SAVE ONLY IF FIRST CHAR IS '@'               
         BE    *+12                                                             
         CLI   8(R2),C'/'           OR '/'                                      
         BNE   BUILD3                                                           
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING CTLOUTD,R6                                                       
         MVI   CTLOCOD,CTLOCODQ                                                 
         MVI   CTLOLEN,CTLOLENQ                                                 
         MVC   CTLOOUT,8(R2)                                                    
         L     R2,EFHDEST                                                       
         MVC   CTLODEST,8(R2)                                                   
         OC    CTLOOUT(16),SPACES                                               
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
BUILD3   L     R2,EFHTAG           VALIDATE DESCRIPTION                         
         BAS   RE,BUMP             PAST 'NAME'                                  
         BAS   RE,BUMP             PAST  NAME                                   
         BAS   RE,BUMP             PAST END OF KEYS OR 'DESC'                   
         TM    1(R2),X'20'         CHECK FOR. . .                               
         BNO   *+8                 . . .NO END OF KEYS FIELD                    
         BAS   RE,BUMP             PAST 'DESC'                                  
         TM    1(R2),X'20'         CHECK FOR DESC                               
         BO    NODESCR                                                          
         CLI   5(R2),0             TEST DESCRIPTION GIVEN                       
         BE    NODESCR             NO                                           
*                                                                               
         MVI   ELCODE,CTLDCODQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING CTLDESCD,R6                                                      
         MVI   CTLDCOD,CTLDCODQ    BUILD DESCRIPTION ELEMENT                    
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CTLDDATA(0),8(R2)                                                
         LA    R1,3(R1)            2 FOR OVERHEAD + 1 FOR BCTR                  
         STC   R1,CTLDLEN                                                       
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         ST    R2,FULL                                                          
         MVI   ELCODE,CTLLCODQ                                                  
         GOTO1 REMELEM                                                          
         CLI   FILTIDNO,0          TEST FILTER MAY BE THERE                     
         BE    BUILD10             NO                                           
*                                                                               
BUILD5   ST    R2,FULL             SAVE A(PREVIOUS FIELD)                       
         BAS   RE,BUMP                                                          
         CLI   0(R2),0             TEST END OF SCREEN                           
         BE    BUILD50             YES                                          
*                                                                               
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    BUILD5              YES -- CHECK NEXT FIELD                      
*                                                                               
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    BUILD10             NO -- THERE'S NO FILTER FIELD                
*                                                                               
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'8'            LENGTH OF EXTENDED HEADER                    
         AR    R1,R2               R1 = A(ID NUMBER)                            
         CLC   FILTIDNO,0(R1)      TEST THIS IS THE FILTER FIELD                
         BNE   BUILD10             NO -- THERE IS NO FILTER FIELD               
*                                                                               
         CLI   5(R2),0             TEST ANY FILTER GIVEN                        
         BE    BUILD10             NO                                           
*                                                                               
         LA    R1,8(R2)            A(BEGINNING OF DATA)                         
         ZIC   R0,5(R2)            INPUT LENGTH                                 
*                                                                               
         CLI   0(R1),C'A'          TEST VALID ALPHANUMERIC                      
         BL    BADFILT                                                          
         LA    R1,1(R1)            CHECK EACH CHARACTER                         
         BCT   R0,*-12                                                          
         ST    R2,FULL             BUMP PAST THIS FIELD LATER                   
*                                                                               
         XC    ELEM,ELEM           BUILD FILTER ELEMENT                         
         LA    R6,ELEM                                                          
         USING CTLFILTD,R6                                                      
         MVI   CTLLCOD,CTLLCODQ                                                 
         MVI   CTLLLEN,CTLLLENQ                                                 
         MVC   CTLLFILT,8(R2)                                                   
         OC    CTLLFILT,SPACES                                                  
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
         EJECT                                                                  
BUILD10  L     R2,FULL             RESTORE A(PREVIOUS FIELD)                    
         MVI   ELCODE,CTLFCODQ                                                  
         GOTO1 REMELEM                                                          
         SR    R3,R3               COUNT NUMBER OF FIELD ELEMENTS               
*                                                                               
BUILD20  BAS   RE,BUMP             REST OF SCREEN                               
         CLI   0(R2),0                                                          
         BE    BUILD50                                                          
*                                                                               
         TM    1(R2),X'20'         PROTECTED?                                   
         BO    BUILD20                                                          
*                                                                               
         TM    1(R2),X'02'         EXTENDED?                                    
         BZ    BUILD20                                                          
*                                                                               
         CLI   5(R2),0             ANY DATA?                                    
         BE    BUILD20                                                          
*                                                                               
         XC    ELEM,ELEM           BUILD FIELD ELEMENT                          
         USING CTLFIELD,R6                                                      
         MVI   CTLFCOD,CTLFCODQ                                                 
         ZIC   R1,0(R2)            GET CODE                                     
         SH    R1,=H'8'                                                         
         IC    R1,0(R1,R2)                                                      
         STC   R1,CTLFID                                                        
*                                                                               
         ZIC   R1,5(R2)            MOVE IN DATA                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CTLFDATA(0),8(R2)                                                
         LA    R1,5(R1)            4 FOR OVERHEAD + 1 FOR BCTR                  
         STC   R1,CTLFLEN                                                       
*                                                                               
         LA    R3,1(R3)            INSERT FIELD SEQ NUMBER                      
         STC   R3,CTLFSEQ                                                       
*                                                                               
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         B     BUILD20                                                          
*                                                                               
BUILD50  GOTO1 GETFACT,DMCB,0      TEST TO PUT SECURITY ELEM IN RECORD          
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'       IS PASSWORD PROTECT ACTIVE                   
         BZ    BUILDX                                                           
         MVC   HALF,FAPASSWD                                                    
         DROP  R1                                                               
*                                                                               
         MVI   ELCODE,CTLPCODQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM           BUILD FIELD ELEMENT                          
E        USING CTLPIDD,ELEM                                                     
         MVI   E.CTLPCOD,CTLPCODQ                                               
         MVI   E.CTLPLEN,CTLPLENQ                                               
         MVC   E.CTLPID,HALF                                                    
         MVC   E.CTLPDATE,BTODAY                                                
         GOTO1 ADDELEM                                                          
         DROP  E                                                                
*                                                                               
BUILDX   B     XIT                                                              
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DREC     DS    0H                                                               
         L     R2,EFHOUT           IF OUTPUT WAS '@' OR '/'...                  
         CLI   8(R2),C'@'           ...CLEAR OUTPUT & DESTINATION               
         BE    *+12                                                             
         CLI   8(R2),C'/'                                                       
         BNE   DREC3                                                            
         XC    8(L'CTLOOUT,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         L     R2,EFHDEST                                                       
         XC    8(L'CTLODEST,R2),8(R2)                                           
         OI    6(R2),X'80'                                                      
*                                                                               
DREC3    L     R6,AIO              OUTPUT & DESTINATION (IF ANY)                
         MVI   ELCODE,CTLOCODQ     FIND ELEMENT                                 
         BAS   RE,GETEL                                                         
         BNE   DREC5                                                            
         USING CTLOUTD,R6                                                       
         L     R2,EFHOUT                                                        
         MVC   8(L'CTLOOUT,R2),CTLOOUT                                          
         OI    6(R2),X'80'                                                      
         L     R2,EFHDEST                                                       
         MVC   8(L'CTLODEST,R2),CTLODEST                                        
         OI    6(R2),X'80'                                                      
         DROP  R6                                                               
*                                                                               
DREC5    L     R6,AIO              DESCRIPTION                                  
         L     R2,EFHTAG                                                        
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         TM    1(R2),X'20'                                                      
         BNO   *+8                                                              
         BAS   RE,BUMP                                                          
         CLI   5(R2),0             CLEAR FIELD                                  
         BE    DREC10                                                           
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
*                                                                               
DREC10   OI    6(R2),X'80'                                                      
         MVI   ELCODE,CTLDCODQ     FIND DESCRIPTION ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   DREC15                                                           
*                                                                               
         USING CTLDESCD,R6                                                      
         ZIC   R1,CTLDLEN                                                       
         SH    R1,=H'2'                                                         
         ZIC   R0,0(R2)            CHECK TOO LONG                               
         SH    R0,=H'8'                                                         
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    R0,=H'8'                                                         
         CR    R0,R1                                                            
         BNL   *+6                                                              
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),CTLDDATA                                                 
         DROP  R6                                                               
*                                                                               
DREC15   ST    R2,FULL                                                          
         CLI   FILTIDNO,0          TEST FILTER MAY BE THERE                     
         BE    DREC25              NO                                           
*                                                                               
DREC20   ST    R2,FULL             SAVE A(PREVIOUS FIELD)                       
         BAS   RE,BUMP                                                          
         CLI   0(R2),0             TEST END OF SCREEN                           
         BE    DREC50              YES                                          
*                                                                               
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    DREC20              YES -- CHECK NEXT FIELD                      
*                                                                               
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    DREC25              NO -- THERE'S NO FILTER FIELD                
*                                                                               
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'8'            LENGTH OF EXTENDED HEADER                    
         AR    R1,R2               R1 = A(ID NUMBER)                            
         CLC   FILTIDNO,0(R1)      TEST THIS IS THE FILTER FIELD                
         BNE   DREC25              NO -- THERE IS NO FILTER FIELD               
*                                                                               
         CLI   5(R2),0             CLEAR FIELD                                  
         BE    DREC23                                                           
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
*                                                                               
DREC23   OI    6(R2),X'80'                                                      
         ST    R2,FULL             BUMP PAST THIS FIELD LATER                   
         L     R6,AIO                                                           
         MVI   ELCODE,CTLLCODQ     FIND FILTER ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DREC25                                                           
*                                                                               
         USING CTLFILTD,R6                                                      
         MVC   8(L'CTLLFILT,R2),CTLLFILT                                        
         DROP  R6                                                               
*                                                                               
DREC25   L     R2,FULL             RESTORE A(FIELD HEADER)                      
         MVI   ELCODE,CTLFCODQ                                                  
*                                                                               
DREC30   BAS   RE,BUMP             REST OF SCREEN                               
         CLI   0(R2),0                                                          
         BE    DREC50                                                           
*                                                                               
         TM    1(R2),X'20'         PROTECTED?                                   
         BO    DREC30                                                           
*                                                                               
         TM    1(R2),X'02'         EXTENDED?                                    
         BZ    DREC30                                                           
*                                                                               
         ZIC   R1,0(R2)            GET CODE                                     
         SH    R1,=H'8'                                                         
         IC    R0,0(R1,R2)                                                      
         STC   R0,BYTE                                                          
         CLC   BYTE,HALF           WANT TO OVERWRITE THIS FIELD?                
         BL    DREC30              NO                                           
*                                                                               
         OI    6(R2),X'80'         CLEAR FIELD                                  
         SH    R1,=H'8'                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
*                                                                               
         L     R6,AIO              FIND ELEMENT                                 
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
DREC40   BAS   RE,NEXTEL           LOOK FOR MORE FIELD ELEMENTS                 
         BNE   DREC30              NO MORE                                      
*                                                                               
         USING CTLFIELD,R6                                                      
         CLC   BYTE,CTLFID         TEST MATCH ON ID NUMBER                      
         BNE   DREC40              NO                                           
*                                                                               
         MVI   CTLFCOD,X'FF'       DON'T USE THIS ELEMENT AGAIN                 
         ZIC   R1,CTLFLEN          FILL FIELD                                   
         SH    R1,=H'4'            GIVES LENGTH OF ELEMENT DATA                 
         ZIC   R0,0(R2)                                                         
         SH    R0,=H'16'           GIVES LENGTH OF FIELD                        
         CR    R0,R1               TEST DATA TOO LONG FOR FIELD                 
         BNL   *+6                 NO                                           
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),CTLFDATA                                                 
*                                                                               
         TM    GENSTAT5,GENPRVAL   SET PREVAL BITS?                             
         BZ    *+8                                                              
         OI    4(R2),X'20'                                                      
*                                                                               
         LA    R1,1(R1)                                                         
         STC   R1,5(R2)            PUT INPUT LENGTH IN HEADER                   
         B     DREC30                                                           
*                                                                               
DREC50   L     R6,AIO              RESTORE IO AREA                              
         MVI   ELCODE,X'FF'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
DREC60   BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
*                                                                               
         MVI   CTLFCOD,CTLFCODQ    RESTORE ELCODE                               
         B     DREC60                                                           
         DROP  R6                                                               
         EJECT                                                                  
* MAKE SURE THAT SECURITY ALLOWS DELETION OF THIS REPORT                        
DELR     MVI   ELCODE,CTLDCODQ     FIND PREVIOUS DESC ELEM                      
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   DELRX                                                            
*                                                                               
         CLI   2(R6),C'('          TEST FIRST CHAR                              
         BNE   DELRX                                                            
* RECORD WAS PROTECTED - OK IF PIDS MATCH                                       
         MVI   ELCODE,CTLPCODQ     FIND PREVIOUS SECURITY ELEM                  
         L     R6,AIO                                                           
         BAS   RE,GETEL            HAVE ONE?                                    
         BNE   DELRX               NOPE                                         
*                                                                               
* GET CURRENT PID (IF ANY) TO SEE IF PIDS MATCH                                 
         USING CTLPIDD,R6                                                       
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'       IS PASSWORD PROTECT ACTIVE                   
         BZ    DELRX               NO - IGNORE SECURITY REQUEST                 
         CLC   FAPASSWD,CTLPID     DOES PERSON MATCH                            
         BE    DELRX                                                            
         MVI   ERROR,SECLOCK       ELSE SECURITY ERROR                          
         B     TRAPERR                                                          
         DROP  R1,R6                                                            
*                                                                               
DELRX    B     XIT                                                              
* VALIDATE AND DISPLAY KEY                                                      
*                                                                               
VKEY     LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING CTLKEYD,R4                                                       
         MVI   CTLKID,CTLKIDQ                                                   
         MVC   CTLAGID,TWAAGY                                                   
         MVC   CTLSYS(3),SVSYSPH                                                
         BAS   RE,OVRSYS                                                        
         L     R2,EFHTAG                                                        
         BAS   RE,BUMP                                                          
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKEYX                                                            
*                                                                               
         GOTO1 ANY                                                              
         CLC   WORK(L'CTLNAME),SPACES                                           
         BNE   *+20                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKEYX                                                            
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         MVC   CTLNAME,WORK                                                     
         DROP  R4                                                               
*                                                                               
         CLI   ACTNUM,ACTSEL                                                    
         BH    VKEYX                                                            
         CLI   ACTNUM,ACTCHA                                                    
         BL    VKEYX                                                            
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    VKEYX                                                            
         MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
VKEYX    B     XIT                                                              
         SPACE 5                                                                
DKEY     LA    R4,KEY              DISPLAY KEY (FOR SELECT)                     
         USING CTLKEYD,R4                                                       
         L     R2,EFHTAG                                                        
         BAS   RE,BUMP                                                          
         MVC   8(8,R2),CTLNAME                                                  
         OI    6(R2),X'80'                                                      
         DROP  R4                                                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LIST     XC    WORK,WORK           BUILD FILTER LIST IN WORK                    
         CLI   FILTIDNO,0          TEST WE SHOULD LOOK AT THE FILTER            
         BE    LIST15              NO                                           
*                                                                               
         L     R2,EFHTAG                                                        
         BAS   RE,BUMP             BUMP PAST 'NAME'                             
         BAS   RE,BUMP             BUMP PAST NAME                               
         BAS   RE,BUMP             BUMP PAST 'FILTER'                           
         CLI   5(R2),0             TEST ANY FILTER GIVEN                        
         BE    LIST15              NO                                           
         CLI   5(R2),8             MAX OF 8 CHARS IN FILTER EXPRESSION          
         BH    BADFILTE                                                         
*                                                                               
         LA    R1,8(R2)                                                         
         LA    R3,WORK             A(FILTER LIST)                               
*                                                                               
LIST5    CLI   0(R1),C' '          TEST END OF FILTER EXPRESSION                
         BNH   LIST15              YES                                          
*                                                                               
         CLI   0(R1),C'*'          TEST WILDCARD                                
         BNE   *+12                NO                                           
         MVI   0(R3),C'*'          PUT '*' INTO WORK                            
         B     LIST10              BRANCH MASK IS ALREADY X'00'                 
*                                                                               
         MVI   1(R3),X'70'         ASSUME IT'S A POSITIVE FILTER                
         CLI   0(R1),C'-'          TEST NEGATIVE FILTER                         
         BNE   *+18                NO                                           
         LA    R1,1(R1)            BUMP PAST MINUS                              
         MVC   0(1,R3),0(R1)       SAVE NEGATIVE FILTER                         
         MVI   1(R3),X'80'         MASK - DO A 'BRANCH EQUAL' LATER             
*                                                                               
         CLI   0(R1),C'A'          TEST ALPHANUMERIC                            
         BL    BADFILTE            NO - ERROR                                   
         MVC   0(1,R3),0(R1)       SAVE FILTER                                  
*                                                                               
LIST10   LA    R1,1(R1)            BUMP PAST FILTER                             
         LA    R3,2(R3)            POINT TO NEXT TABLE ENTRY                    
         B     LIST5                                                            
*                                                                               
LIST15   MVI   NLISTS,16                                                        
         LA    R4,KEY                                                           
         USING CTLKEYD,R4                                                       
         TM    GENSTAT2,DISTHSPG   IS RE-DISPLAY PAGE OPTION ON?                
         BNO   LIST15A                                                          
         CLI   LISTSW,C'T'         IS OPTION TO BE DONE NOW?                    
         BNE   LIST15A                                                          
         XC    KEY,KEY                                                          
         MVC   CTLKEYD(25),LISTKEYS     USE FIRST KEY ON SCREEN                 
         B     LIST17                                                           
LIST15A  DS    0H                                                               
         OC    KEY,KEY             WAS A KEY PROVIDED?                          
         BNZ   LIST20              YES-LAST KEY ON PREVIOUS SCREEN              
         MVI   CTLKID,CTLKIDQ                                                   
         MVC   CTLAGID,TWAAGY                                                   
         MVC   CTLSYS(3),SVSYSPH                                                
         BAS   RE,OVRSYS                                                        
         L     R2,EFHTAG                                                        
         BAS   RE,BUMP             BUMP TO NAME                                 
         CLI   5(R2),0                                                          
         BE    LIST17              NO NAME GIVEN                                
*                                                                               
         MVC   CTLNAME,SPACES                                                   
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CTLNAME(0),8(R2)                                                 
*                                                                               
LIST17   GOTO1 HIGH                                                             
         B     LIST25                                                           
*                                                                               
LIST20   GOTO1 SEQ                                                              
*                                                                               
LIST25   CLC   KEY(17),KEYSAVE     TEST RECORD IS FOR THIS PROGRAM              
         BNE   LISTX               NO                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CTLLCODQ     SEE IF THERE'S A FILTER ELEMENT              
         BAS   RE,GETEL                                                         
         BE    *+12                YES - POINT R6 TO THE FILTER                 
         LA    R6,SPACES           NO - POINT R6 TO BLANKS                      
         B     *+8                                                              
*                                                                               
         USING CTLFILTD,R6                                                      
         LA    R6,CTLLFILT                                                      
         DROP  R6                                                               
*                                                                               
         LA    R3,WORK             FILTER EXPRESSION TABLE                      
         LA    R0,4                MAXIMUM OF FOUR FILTER POSITIONS             
*                                                                               
LIST30   CLI   0(R3),0             TEST END OF TABLE                            
         BE    LIST35              YES - WE WANT THIS ONE                       
         ZIC   R1,1(R3)            BRANCH MASK                                  
         CLC   0(1,R3),0(R6)       TEST MATCH ON FILTER. . .                    
         EX    R1,*+8              . . . WITH THE PROPER CONDITION              
         B     *+8                                                              
         BC    0,LIST20            FILTER FAILED - GET ANOTHER RECORD           
         LA    R3,2(R3)            BUMP TO NEXT ENTRY IN TABLE                  
         LA    R6,1(R6)            BUMP TO NEXT FILTER CHARACTER                
         BCT   R0,LIST30           UP TO FOUR FILTERS                           
*                                                                               
LIST35   MVC   LISTAR,SPACES                                                    
         LA    R3,LISTAR                                                        
*                                                                               
         LA    R3,1(R3)            NAME                                         
         MVC   0(8,R3),CTLNAME                                                  
*                                                                               
         LA    R3,13(R3)           DESCRIPTION                                  
         L     R6,AIO                                                           
         MVI   ELCODE,CTLDCODQ                                                  
         BAS   RE,GETEL                                                         
         BNE   LIST40                                                           
*                                                                               
         USING CTLDESCD,R6                                                      
         ZIC   R1,CTLDLEN                                                       
         SH    R1,=H'2'                                                         
         CH    R1,=H'34'                                                        
         BNH   *+8                                                              
         LH    R1,=H'34'                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),CTLDDATA                                                 
         DROP  R4,R6                                                            
*                                                                               
LIST40   GOTO1 LISTMON                                                          
         B     LIST20                                                           
*                                                                               
LISTX    B     XIT                                                              
         EJECT                                                                  
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
         SPACE 2                                                                
OVRSYS   CLC   SVSYSPH(2),=X'0325' BE REALLY CAREFUL                            
         BNE   OVRSYS2              (ONLY FOR POD REPORTS)                      
         USING CTLKEYD,R4                                                       
         CLI   ALTSYS,0            SYSTEM OVERRIDE PRESENT                      
         BER   RE                   NO - GET OUT                                
         MVC   CTLSYS(1),ALTSYS     YES - USE IT                                
         BR    RE                                                               
OVRSYS2  CLC   SVSYSPH(2),=X'0204' SPOT WRITER CAN GET PHASE OVERRIDE           
         BNER  RE                                                               
         CLI   ALTPHS,0                                                         
         BER   RE                                                               
         MVC   CTLSYS+2(1),ALTPHS                                               
         BR    RE                                                               
         DROP  R4                                                               
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
NODESCR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NODESCRM),NODESCRM                                     
         GOTO1 ERREX2                                                           
NODESCRM DC    C'* ERROR * DESCRIPTION REQUIRED *'                              
         SPACE 2                                                                
BADFILT  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADFILTM),BADFILTM                                     
         GOTO1 ERREX2                                                           
BADFILTM DC    C'* ERROR * INVALID FILTER *'                                    
         SPACE 2                                                                
BADFILTE XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADFILEM),BADFILEM                                     
         GOTO1 ERREX2                                                           
BADFILEM DC    C'* ERROR * INVALID FILTER EXPRESSION *'                         
         SPACE 2                                                                
TRAPERR  GOTO1 ERREX                                                            
         B     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDGENFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
************************************************************                    
*       NOTE NOTE NOTE                                     *                    
* THERE IS A PAN BOOK CTGENPGREC OF THIS DSECT             *                    
*                                                          *                    
* IT ALREADY APPEARS THAT THE X'20' ELEMENTS DON'T AGREE.  *                    
* AS FAR AS I KNOW, THE 20 ELEM IN HERE IS IN USE,         *                    
* I DON'T KNOW ABOUT THE ONE IN CTGENPGREC                 *                    
*                                                          *                    
************************************************************                    
         SPACE                                                                  
CTLKEYD  DSECT                                                                  
CTLKID   DS    XL1                                                              
CTLKIDQ  EQU   X'01'                                                            
         DS    CL11                                                             
CTLAGID  DS    CL2                                                              
CTLSYS   DS    XL1                                                              
CTLPRG   DS    XL1                                                              
CTLPHAS  DS    XL1                                                              
CTLNAME  DS    CL8                                                              
CTLLEN   DS    XL2                                                              
CTLSTAT  DS    XL1                                                              
CTLDATA  DS    0X                                                               
*                                                                               
CTLDESCD DSECT                                                                  
CTLDCOD  DS    XL1                                                              
CTLDCODQ EQU   X'01'                                                            
CTLDLEN  DS    XL1                                                              
CTLDDATA DS    0X                                                               
*                                                                               
CTLFIELD DSECT                                                                  
CTLFCOD  DS    XL1                                                              
CTLFCODQ EQU   X'02'                                                            
CTLFLEN  DS    XL1                                                              
CTLFID   DS    XL1                                                              
CTLFSEQ  DS    XL1                                                              
CTLFDATA DS    0X                                                               
*                                                                               
CTLFILTD DSECT                                                                  
CTLLCOD  DS    XL1                                                              
CTLLCODQ EQU   X'10'                                                            
CTLLLEN  DS    XL1                                                              
CTLLFILT DS    CL4                                                              
CTLLLENQ EQU   *-CTLFILTD                                                       
*                                                                               
* THE FOLLOWING ONLY PRESENT IF OUTPUT FIELD STARTS WITH '@' OR '/'             
CTLOUTD  DSECT         OUTPUT & DEST FIELDS                                     
CTLOCOD  DS    XL1                                                              
CTLOCODQ EQU   X'20'                                                            
CTLOLEN  DS    XL1                                                              
CTLOOUT  DS    CL8                                                              
CTLODEST DS    CL8                                                              
CTLOLENQ EQU   *-CTLOUTD                                                        
*                                                                               
CTLPIDD  DSECT         SECURITY ELEMENT                                         
CTLPCOD  DS    XL1                                                              
CTLPCODQ EQU   X'FE'                                                            
CTLPLEN  DS    XL1                                                              
CTLPID   DS    XL2                 PERSONAL ID OF LAST TO CHANGE                
CTLPDATE DS    XL3                 DATE OF LAST CHANGE                          
CTLPLENQ EQU   *-CTLPIDD                                                        
       ++INCLUDE FAFACTS                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021GEGENPRG  01/20/04'                                      
         END                                                                    
