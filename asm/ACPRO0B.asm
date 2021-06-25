*          DATA SET ACPRO0B    AT LEVEL 009 AS OF 09/12/02                      
*PHASE T60B0BA,*                                                                
         TITLE 'T60B0B - OFFICE GROUP MAINT'                                    
T60B0B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B0B**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         GOTO1 VMODPTRS,DMCB,(X'80',POINTERS)                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         CLI   ACTNUM,ACTREP       SKIP KEY VALIDATION                          
         BE    *+8                 FOR ACTION REPORT                            
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,VALREC                                                      
         BNE   MODE4                                                            
         BAS   RE,VREC                                                          
         BAS   RE,DKEY                                                          
         BAS   RE,DREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE4    CLI   MODE,DISPKEY                                                     
         BNE   MODE6                                                            
         BAS   RE,DKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE6    CLI   MODE,DISPREC                                                     
         BNE   MODE8                                                            
         BAS   RE,DREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE8    CLI   MODE,PRINTREP                                                    
         BNE   MODE10                                                           
         BAS   RE,PREP                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE10   GOTO1 CANWEDEL                                                         
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
         SPACE 3                                                                
VKEY     NTR1                      OFFICE GROUP                                 
         LA    R2,PROOGRH                                                       
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),1                                                          
         BNE   ERREND                                                           
         LA    R4,KEY                                                           
         USING ACOGKEY,R4                                                       
         XC    ACOGKEY,ACOGKEY                                                  
         MVI   ACOGRTYP,ACOGEQU                                                 
         MVI   ACOGSREC,ACOGOG                                                  
         MVC   ACOGCUL,CUL                                                      
         MVC   ACOGCODE,WORK                                                    
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 3                                                                
VREC     NTR1                                                                   
         LA    R2,PROOGRNH                                                      
         GOTO1 ANY                                                              
         GOTO1 NAMEIN                                                           
         GOTO1 PERSIN                                                           
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY KEY                                                      
         SPACE 3                                                                
DKEY     NTR1                                                                   
         L     R4,AIO                                                           
         USING ACOGKEY,R4                                                       
         MVC   PROOGR,ACOGCODE                                                  
         LA    R2,PROOGRH                                                       
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY RECORD                                                   
         SPACE 3                                                                
DREC     NTR1                                                                   
         LA    R2,PROOGRNH                                                      
         GOTO1 NAMEOUT                                                          
         GOTO1 PERSOUT                                                          
         MVC   PROLACT,SPACES                                                   
         MVC   PROLACT(14),=C'LAST ACTIVITY:'                                   
         MVC   PROLACT+15(20),WORK+20                                           
         OI    PROLACTH+6,X'80'                                                 
         B     XIT                                                              
         EJECT                                                                  
*              PRINT OFFICE GROUP REPORT                                        
         SPACE 3                                                                
PREP     NTR1                                                                   
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
*                                                                               
         XC    NENTS,NENTS                                                      
         GOTO1 CALLOV,DMCB,0,X'D9000A50'                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   QSORT,DMCB                                                       
*                                                                               
PREP1    LA    R4,KEY                                                           
         USING ACOGKEY,R4                                                       
         XC    ACOGKEY,ACOGKEY     READ OFFICE GROUP RECORDS                    
         MVI   ACOGRTYP,ACOGEQU                                                 
         MVI   ACOGSREC,ACOGOG     OFFICE GROUP SUB-RECORD                      
         MVC   ACOGCUL,CUL                                                      
         GOTO1 HIGH                                                             
         B     PREP4                                                            
         SPACE 1                                                                
PREP2    GOTO1 SEQ                                                              
         SPACE 1                                                                
PREP4    CLC   ACOGKEY(ACOGCODE-ACOGKEY),KEYSAVE   CHECK C/B                    
         BNE   PREP6                                                            
*                                                                               
         XC    SORTREC,SORTREC                                                  
         LA    R3,SORTREC                                                       
         USING SORTD,R3                                                         
         MVC   SORTOG,ACOGCODE                                                  
         GOTO1 SETNAME,DMCB,AIO,WORK                                            
         MVC   SORTNAME,WORK                                                    
         BAS   RE,ADDBUFF                                                       
         B     PREP2                                                            
*                                                                               
PREP6    LA    R4,KEY                                                           
         XC    ACOGKEY,ACOGKEY     READ OFFICE RECORDS                          
         MVI   ACOGRTYP,ACOGEQU                                                 
         MVI   ACOGSREC,ACOGOFF    OFFICE SUB-RECORD                            
         MVC   ACOGCUL,CUL                                                      
         GOTO1 HIGH                                                             
         B     PREP8                                                            
         SPACE 1                                                                
PREP7    GOTO1 SEQ                                                              
         SPACE 1                                                                
PREP8    CLC   ACOGKEY(ACOGCODE-ACOGKEY),KEYSAVE  CHECK C/B                     
         BNE   PREP10                                                           
         SPACE 1                                                                
         MVI   ELCODE,ACGPELQ      GET PRODUCTION GROUP ELEMENT                 
         BAS   RE,GETELIO                                                       
         BNE   PREP7               NOT IN A GROUP                               
         USING ACGPD,R6                                                         
         XC    SORTREC,SORTREC                                                  
         LA    R3,SORTREC                                                       
         MVC   SORTOG,ACGPCODE     EXTRACT OFFICE GROUP                         
         MVC   SORTOFF,ACOGOFC     EXTRACT OFFICE                               
         GOTO1 SETNAME,DMCB,AIO,WORK                                            
         MVC   SORTNAME,WORK       EXTRACT OFFICE NAME                          
         BAS   RE,ADDBUFF                                                       
         B     PREP7               GET NEXT OFFICE                              
*                                                                               
PREP10   MVI   PENDSW,C'N'         SET PENDING SWITCH TO NO                     
         LA    R3,BUFF             R3=A(SORT RECORD BUFFER)                     
         USING SORTD,R3                                                         
         ICM   R4,15,NENTS                                                      
         BZ    PREPX               NOTHING TO PRINT                             
         GOTO1 QSORT,DMCB,(R3),(R4),SORTRECL,L'SORTKEY,0                        
*                                                                               
PREP12   LA    R2,P                                                             
         USING PRTD,R2                                                          
         OC    SORTOFF,SORTOFF     TEST FOR OFFICE RECORD                       
         BNZ   PREP14              YES                                          
         CLI   PENDSW,C'Y'         TEST FOR PENDING OFFICE GROUP                
         BNE   PREP13                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PREP13   GOTO1 SPOOL,DMCB,(R8)     SKIP A LINE BEFORE                           
         MVC   PRTOG,SORTOG        EXTRACT OFFICE GROUP                         
         MVC   PRTNAME,SORTNAME    AND OFFICE GROUP NAME                        
         MVI   PENDSW,C'Y'         SET PENDING OFFICE GROUP LINE                
         B     PREP15              GET NEXT RECORD                              
*                                                                               
PREP14   MVC   PRTOFF,SORTOFF      EXTRACT OFFICE CODE                          
         MVC   PRTOFFN,SORTNAME    AND NAME                                     
         CLI   PENDSW,C'Y'         TEST PENDING MEDIA GROUP                     
         BNE   *+12                                                             
         MVI   ALLOWLIN,2          ALLOW FOR 2 OFFICE LINES                     
         MVI   PENDSW,C'N'         TURN SWITCH OFF                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PREP15   LA    R3,SORTRECL(R3)     NEXT ENTRY                                   
         BCT   R4,PREP12                                                        
         CLI   PENDSW,C'Y'         TEST FOR PENDING GROUP LINE                  
         BNE   PREPX                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PREPX    B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO ADD AN ENTRY AT SORTREC TO SORT BUFFER                         
*                                                                               
ADDBUFF  LA    R1,BUFF                                                          
         L     R0,NENTS                                                         
         LR    RF,R0                                                            
         MH    R0,=Y(SORTRECL)     COMPUTE DISP TO NEXT ENTRY                   
         AR    R1,R0               R1=A(NEXT ENTRY)                             
         LA    RF,1(RF)                                                         
         C     RF,=A(MAXENTS)                                                   
         BNH   *+6                                                              
         DC    H'0'                                                             
         ST    RF,NENTS                                                         
         MVC   0(SORTRECL,R1),SORTREC                                           
         BR    RE                                                               
         EJECT                                                                  
*              HEAD HOOK                                                        
         SPACE 3                                                                
HOOK     NTR1                                                                   
         L     R4,ABOX                                                          
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         CLI   BOXOPT,C'N'                                                      
         BE    XIT                                                              
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,0                                                         
         MVI   BOXINIT,0                                                        
         MVI   BOXWT,1                                                          
         MVC   BOXCOLS,SPACES                                                   
         LA    R2,BOXCOLS                                                       
         MVI   PRTLBOX-PRTD(R2),C'L'                                            
         MVI   PRTBOX1-PRTD(R2),C'C'                                            
         MVI   PRTBOX2-PRTD(R2),C'C'                                            
         MVI   PRTBOX3-PRTD(R2),C'C'                                            
         MVI   PRTRBOX-PRTD(R2),C'R'                                            
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
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
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
*              SPECS FOR HEADINGS ETC                                           
         SPACE 1                                                                
MYSPECS  DS    0F                                                               
         SSPEC H1,2,CREATED        SPECS FOR REGULAR PRINTING                   
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,44,C'OFFICE GROUP LISTING'                                    
         SSPEC H2,44,C'--------------------'                                    
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,REPORT                                                     
         SSPEC H4,110,PAGE                                                      
*                                                                               
         SSPEC H8,2,C'OFFICE'                                                   
         SSPEC H9,2,C'GROUP'                                                    
         SSPEC H8,10,C'OFFICE GROUP NAME'                                       
         SSPEC H8,52,C'OFFICE'                                                  
         SSPEC H9,52,C'CODE'                                                    
         SSPEC H8,60,C'OFFICE NAME'                                             
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECTS ARE HIDDEN IN HERE                                        
         SPACE 3                                                                
*DDSPOOLD                                                                       
         PRINT  OFF                                                             
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT  OFF                                                             
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT  OFF                                                             
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT  OFF                                                             
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROFBD                                                       
         SPACE 2                                                                
QSORT    DS    V                                                                
NENTS    DS    F                                                                
SORTREC  DS    CL(SORTRECL)                                                     
PENDSW   DS    C                                                                
POINTERS DS    CL(8*54+1)          PASSIVE POINTER BLOCK                        
         SPACE 2                                                                
* DSECT TO COVER SORT RECORD                                                    
*                                                                               
SORTD    DSECT                                                                  
SORTKEY  DS    0CL3                                                             
SORTOG   DS    C                                                                
SORTOFF  DS    CL2                                                              
SORTDATA DS    0CL36                                                            
SORTNAME DS    CL36                                                             
SORTRECL EQU   *-SORTD                                                          
         SPACE 2                                                                
* DSECT TO COVER PRINT LINE                                                     
*                                                                               
PRTD     DSECT                                                                  
PRTLBOX  DS    C                   LEFT HAND BOX                                
PRTOG    DS    C                   OFFICE GROUP                                 
         DS    CL6                                                              
PRTBOX1  DS    C                                                                
PRTNAME  DS    CL36                OFFICE GROUP NAME                            
         DS    CL5                                                              
PRTBOX2  DS    C                                                                
PRTOFF   DS    CL2                 OFFICE CODE                                  
         DS    CL6                                                              
PRTBOX3  DS    C                                                                
PRTOFFN  DS    CL36                OFFICE NAME                                  
         DS    C                                                                
PRTRBOX  DS    C                                                                
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
MAXENTS  EQU   (SYSEND-BUFF)/SORTRECL                                           
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACPRO0B   09/12/02'                                      
         END                                                                    
