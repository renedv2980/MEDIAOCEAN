*          DATA SET ACPRO06    AT LEVEL 020 AS OF 05/26/15                      
*PHASE T60B06A                                                                  
         TITLE 'T60B06 - USER FIELD SELECT MAINT'                               
T60B06   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B06**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         GOTO1 VMODPTRS,DMCB,(X'80',POINTERS)                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
MODE2    CLI   MODE,VALREC                                                      
         BNE   MODE4                                                            
         BAS   RE,VREC                                                          
         BAS   RE,DKEY                                                          
         BAS   RE,DREC                                                          
         B     XIT                                                              
*                                                                               
MODE4    CLI   MODE,DISPKEY                                                     
         BNE   MODE6                                                            
         BAS   RE,DKEY                                                          
         B     XIT                                                              
*                                                                               
MODE6    CLI   MODE,DISPREC                                                     
         BNE   MODE8                                                            
         BAS   RE,DREC                                                          
         B     XIT                                                              
*                                                                               
MODE8    GOTO1 CANWEDEL                                                         
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
         SPACE 3                                                                
VKEY     NTR1                                                                   
         LA    R4,USERKEY                                                       
         USING ACUFKEY,R4                                                       
         XC    USERKEY,USERKEY                                                  
         MVI   ACUFRTYP,ACUFEQU                                                 
         MVI   ACUFSREC,ACUFSEQU                                                
         MVC   ACUFCUL,CUL                                                      
*                                                                               
         LA    R2,PROOGRH         OFFICE GROUP                                  
         CLI   5(R2),0                                                          
         BE    VKEY2                                                            
         CLC   =C'ALL',8(R2)       TEST FOR ALL                                 
         BE    VKEY2                                                            
         GOTO1 VALOG                                                            
         MVC   ACUFOG,8(R2)                                                     
*                                                                               
VKEY2    LA    R2,PROOFFH          OFFICE                                       
         CLI   5(R2),0                                                          
         BE    VKEY4                                                            
         MVI   ERROR,NOTOFNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   PROOGRH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALOFF                                                           
         MVC   ACUFOFC,EFFOFFC                                                  
*                                                                               
VKEY4    LA    R2,PROCLIH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    VKEY6                                                            
         MVI   ERROR,NOTCLNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   PROOGRH+5,0                                                      
         BNE   ERREND                                                           
         MVI   ERROR,NOTCLNOF      NOT COMPATIBLE WITH OFFICE                   
         CLI   PROOFFH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALCLI                                                           
         MVC   ACUFCLI,CLICODE                                                  
*                                                                               
VKEY6    LA    R2,PROPROH          PRODUCT                                      
         CLI   5(R2),0                                                          
         BE    VKEY8                                                            
         MVI   ERROR,NEEDCLI       IF INPUT, NEED CLIENT AS WELL                
         CLI   PROCLIH+5,0                                                      
         BE    ERREND                                                           
         GOTO1 VALPROD                                                          
         MVC   ACUFPRO,PRODCODE                                                 
*                                                                               
VKEY8    LA    R2,PROMGRH          MEDIA GROUP                                  
         CLI   5(R2),0                                                          
         BE    VKEY10                                                           
         GOTO1 VALMG                                                            
         MVC   ACUFMG,8(R2)                                                     
*                                                                               
VKEY10   LA    R2,PROMEDH          MEDIA                                        
         CLI   5(R2),0                                                          
         BE    VKEY12                                                           
         MVI   ERROR,NOTMENMG      NOT COMPATIBLE WITH MEDIA GROUP              
         CLI   PROMGRH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALMED                                                           
         MVC   ACUFMED,8(R2)                                                    
*                                                                               
VKEY12   LA    R2,PROJOBH          JOB                                          
         CLI   5(R2),0                                                          
         BE    VKEYEND                                                          
         MVI   ERROR,NEEDPRO       IF INPUT, NEED PRODUCT AS WELL               
         CLI   PROPROH+5,0                                                      
         BE    ERREND                                                           
         MVI   ERROR,NOTJBNME      NOT COMPATIBLE WITH MEDIA                    
         CLI   PROMGRH+5,0                                                      
         BNE   ERREND                                                           
         CLI   PROMEDH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALJOB                                                           
         MVC   ACUFJOB,JOBNUM                                                   
*                                                                               
VKEYEND  MVC   KEY,USERKEY                                                      
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 3                                                                
VREC     NTR1                                                                   
         MVI   ELCODE,X'A2'        REMOVE EXISTING A2 ELEMENTS                  
         GOTO1 REMELEM                                                          
         LA    R2,PROFLDH                                                       
         LA    R3,1                SEQUENCE NUMBER IN R3                        
         LA    R5,10               MAXIMUM NUMBER OF LINES                      
*                                                                               
VREC2    LA    R3,1(R3)                                                         
         LR    R4,R2               SAVE THIS SPOT                               
         CLI   5(R2),0             FIELD CODE                                   
         BE    VREC10                                                           
         LA    R6,ELEMENT                                                       
         USING ACUFD,R6                                                         
         XC    ELEMENT,ELEMENT                                                  
         MVI   ACUFEL,X'A2'                                                     
         MVI   ACUFLEN,32                                                       
         GOTO1 ANY                                                              
         MVC   ACUFCODE,WORK                                                    
*                                                                               
         BAS   RE,BUMP             FIELD HEADER                                 
         GOTO1 ANY                                                              
         MVC   ACUFDESC,WORK                                                    
*                                                                               
         BAS   RE,BUMP             EDIT RULE                                    
         CLI   5(R2),0                                                          
         BE    VREC4                                                            
         MVC   ACUFEDIT(1),8(R2)                                                
         CLI   ACUFEDIT,C'D'                                                    
         BE    VREC4                                                            
         CLI   ACUFEDIT,C'N'                                                    
         BE    VREC4                                                            
         CLI   ACUFEDIT,C'C'                                                    
         BE    VREC4                                                            
         MVI   ERROR,BADEDIT                                                    
         B     ERREND                                                           
*                                                                               
VREC4    BAS   RE,BUMP             MAX LENGTH                                   
         CLI   ACUFEDIT,C'D'                                                    
         BNE   VREC4A                                                           
         MVI   ACUFMXLN,8          DATE MUST BE 8 BYTES                         
         B     VREC4B                                                           
*                                                                               
VREC4A   GOTO1 VALINUM                                                          
         CLI   ACTUAL,30           TEST 1-30                                    
         BH    INVEND                                                           
         MVC   ACUFMXLN,ACTUAL                                                  
*                                                                               
VREC4B   BAS   RE,BUMP             REQUIRED FIELD                               
         CLI   8(R2),C'Y'                                                       
         BNE   *+8                                                              
         OI    ACUFSTAT,X'80'                                                   
*                                                                               
         BAS   RE,BUMP             NEED FOR BILLS                               
         CLI   8(R2),C'Y'                                                       
         BNE   *+8                                                              
         OI    ACUFSTAT,X'40'                                                   
*                                                                               
         BAS   RE,BUMP             SHOW ON ESTIMATE                             
         CLI   8(R2),C'Y'                                                       
         BNE   *+8                                                              
         OI    ACUFSTAT,X'20'                                                   
*                                                                               
         BAS   RE,BUMP             SHOW ON BILLS                                
         CLI   8(R2),C'Y'                                                       
         BNE   *+8                                                              
         OI    ACUFSTAT,X'10'                                                   
*                                                                               
         BAS   RE,BUMP             NEED FOR RECEIVABLE                          
         CLI   8(R2),C'Y'                                                       
         BNE   *+8                                                              
         OI    ACUFSTAT,X'08'                                                   
*                                                                               
         BAS   RE,BUMP             SHOW ON AUTHORIZATION                        
         CLI   8(R2),C'Y'                                                       
         BNE   *+8                                                              
         OI    ACUFSTAT,X'04'                                                   
*                                                                               
         BAS   RE,BUMP             CUTOFF DATE                                  
         CLI   5(R2),0                                                          
         BE    VREC5                                                            
         MVI   ERROR,INVDATE                                                    
         GOTO1 DATVAL,DMCB,8(R2),DUB                                            
         OC    DMCB,DMCB                                                        
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,(0,DUB),(1,ACUFCUT)                                  
*                                                                               
VREC5    GOTO1 HELLO,DMCB,(C'G',SYSFIL),(X'A2',AIO),(4,ELEMENT+2)               
         CLI   DMCB+12,0           IF RECORD FOUND PRINT ERROR AND EXIT         
         BE    VREC8                                                            
         MVI   ERROR,TOOLONG       EXIT IF RECORD IS NOW TOO LONG               
         CLI   DMCB+12,5                                                        
         BE    ERREND                                                           
         CLI   DMCB+12,6           BLOW UP IF ANYTHING OTHER THAN               
         BE    *+6                  RECORD NOT FOUND                            
         DC    H'0'                                                             
         GOTO1 ADDELEM                                                          
VREC6    CH    R3,=H'10'                                                        
         BH    VREC14                                                           
         BAS   RE,BUMP                                                          
         BCT   R5,VREC2                                                         
         B     VREC14                                                           
*                                                                               
VREC8    MVI   ERROR,DUPFIELD                                                   
VREC9    LR    R2,R4                                                            
         B     ERREND                                                           
*                                                                               
VREC10   CLI   ACTNUM,ACTADD       IF WE ARE ADDING, BLANKING A                 
         BNE   VREC12               FIELD WILL WORK                             
         CH    R3,=H'10'                                                        
         BH    VREC14                                                           
         BAS   RE,NEXTLINE                                                      
         BCT   R5,VREC2                                                         
         B     VREC14                                                           
*                                                                               
VREC12   BAS   RE,NEXTLINE         IF WE ARE CHANGING, IT WILL NOT              
         MVI   ERROR,MISSING                                                    
         CLI   MOREDATA,C'Y'                                                    
         BE    VREC9                                                            
         BAS   RE,BUMP                                                          
         BCT   R5,VREC2                                                         
*                                                                               
VREC14   MVI   ELCODE,X'A2'        DO WE HAVE ANY ELEMENTS ?                    
         GOTO1 GETELIO                                                          
         BE    VRECEND             YES                                          
         CLI   ACTNUM,ACTADD       IF NO ELS FOR ACTION ADD---ERROR             
         BNE   VREC14A                                                          
         LA    R2,PROFLDH          SET TO FIRST FIELD                           
         MVI   ERROR,MISSING       RETURN AN ERROR                              
         B     ERREND                                                           
VREC14A  L     R4,AIO              NO, FLAG RECORD FOR DELETE                   
         USING ACKEYD,R4                                                        
         OI    ACSTATUS,X'80'                                                   
         B     XIT                                                              
*                                                                               
VRECEND  GOTO1 PERSIN                                                           
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY KEY                                                      
         SPACE 3                                                                
DKEY     NTR1                                                                   
         L     R4,AIO                                                           
         USING ACUFKEY,R4                                                       
         MVC   PROOGR(L'ACUFOG),ACUFOG                                          
         OI    PROOGRH+6,X'80'                                                  
         MVC   PROOFF,ACUFOFC                                                   
         OI    PROOFFH+6,X'80'                                                  
         MVC   PROCLI,ACUFCLI                                                   
         OI    PROCLIH+6,X'80'                                                  
         MVC   PROPRO,ACUFPRO                                                   
         OI    PROPROH+6,X'80'                                                  
         MVC   PROMGR,ACUFMG                                                    
         OI    PROMGRH+6,X'80'                                                  
         MVC   PROMED,ACUFMED                                                   
         OI    PROMEDH+6,X'80'                                                  
         MVC   PROJOB,ACUFJOB                                                   
         OI    PROJOBH+6,X'80'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY RECORD                                                   
         SPACE 3                                                                
DREC     NTR1                                                                   
         GOTO1 PERSOUT                                                          
         MVC   PROLACT,SPACES                                                   
         MVC   PROLACT(14),=C'LAST ACTIVITY:'                                   
         MVC   PROLACT+15(20),WORK+20                                           
         OI    PROLACTH+6,X'80'                                                 
*                                                                               
DREC2    GOTO1 VCLEARF,DMCB,PROFLDH,PROLAST                                     
*                                                                               
DREC4    LA    R2,PROFLDH                                                       
         MVI   ELCODE,X'A2'                                                     
         GOTO1 GETELIO                                                          
         USING ACUFEL,R6                                                        
*                                                                               
DREC6    BNE   XIT                                                              
         MVC   8(2,R2),ACUFCODE                                                 
         BAS   RE,BUMP                                                          
         MVC   8(12,R2),ACUFDESC                                                
         BAS   RE,BUMP                                                          
         MVC   8(8,R2),ACUFEDIT                                                 
         BAS   RE,BUMP                                                          
         EDIT  (1,ACUFMXLN),(2,8(R2)),ALIGN=LEFT                                
         BAS   RE,BUMP                                                          
         MVI   8(R2),C'N'                                                       
         TM    ACUFSTAT,X'80'                                                   
         BNO   *+8                                                              
         MVI   8(R2),C'Y'                                                       
         BAS   RE,BUMP                                                          
         MVI   8(R2),C'N'                                                       
         TM    ACUFSTAT,X'40'                                                   
         BNO   *+8                                                              
         MVI   8(R2),C'Y'                                                       
         BAS   RE,BUMP                                                          
         MVI   8(R2),C'N'                                                       
         TM    ACUFSTAT,X'20'                                                   
         BNO   *+8                                                              
         MVI   8(R2),C'Y'                                                       
         BAS   RE,BUMP                                                          
         MVI   8(R2),C'N'                                                       
         TM    ACUFSTAT,X'10'                                                   
         BNO   *+8                                                              
         MVI   8(R2),C'Y'                                                       
         BAS   RE,BUMP                                                          
         MVI   8(R2),C'N'                                                       
         TM    ACUFSTAT,X'08'                                                   
         BNO   *+8                                                              
         MVI   8(R2),C'Y'                                                       
         BAS   RE,BUMP                                                          
         MVI   8(R2),C'N'                                                       
         TM    ACUFSTAT,X'04'                                                   
         BNO   *+8                                                              
         MVI   8(R2),C'Y'                                                       
         BAS   RE,BUMP                                                          
         OC    ACUFCUT,ACUFCUT                                                  
         BZ    DREC7                                                            
         GOTO1 DATCON,DMCB,(1,ACUFCUT),(8,8(R2))                                
*                                                                               
DREC7    BAS   RE,BUMP                                                          
         BAS   RE,NEXTEL                                                        
         B     DREC6                                                            
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
NEXTLINE LA    R0,10               NUMBER OF TIMES TO BUMP                      
         MVI   MOREDATA,C'N'                                                    
BUMPNXT  ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         MVI   MOREDATA,C'Y'                                                    
         BCT   R0,BUMPNXT                                                       
         BR    RE                                                               
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 3                                                                
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
*                                                                               
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
INVEND   MVI   ERROR,INVALID       INVALID EXIT                                 
         B     ERREND                                                           
*                                                                               
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
*                                                                               
ERREND   GOTO1 VERRCUR                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECTS ARE HIDDEN IN HERE                                        
         SPACE 3                                                                
*ACPROWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROF6D                                                       
         DS    0F                                                               
USERKEY  DS    CL64                                                             
MOREDATA DS    CL1                                                              
POINTERS DS    XL(8*54+1)                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020ACPRO06   05/26/15'                                      
         END                                                                    
