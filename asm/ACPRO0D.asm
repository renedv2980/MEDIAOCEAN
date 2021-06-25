*          DATA SET ACPRO0D    AT LEVEL 007 AS OF 09/12/02                      
*PHASE T60B0DA,*                                                                
         TITLE 'T60B0D - WORK GROUP MAINT'                                      
T60B0D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B0D**,RA                                                    
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
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         CLI   ACTNUM,ACTREP       SKIP KEY FIELDS VALIDATE                     
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
VKEY     NTR1                      MEDIA GROUP                                  
         LA    R2,PROWGRH                                                       
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),1                                                          
         BNE   ERREND                                                           
         LA    R4,KEY                                                           
         USING ACWGKEY,R4                                                       
         XC    ACWGKEY,ACWGKEY                                                  
         MVI   ACWGRTYP,ACWGEQU                                                 
         MVI   ACWGSREC,ACWGSEQU                                                
         MVC   ACWGCUL,CUL                                                      
         MVC   ACWGCODE,WORK                                                    
         B     XIT                                                              
         EJECT                                                                  
*              VADIDATE RECORD                                                  
         SPACE 3                                                                
VREC     NTR1                                                                   
         LA    R2,PROWGRNH                                                      
         GOTO1 ANY                                                              
         GOTO1 NAMEIN                                                           
         GOTO1 PERSIN                                                           
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY KEY                                                      
         SPACE 3                                                                
DKEY     NTR1                                                                   
         L     R4,AIO                                                           
         USING ACWGKEY,R4                                                       
         MVC   PROWGR,ACWGCODE                                                  
         LA    R2,PROWGRH                                                       
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY RECORD                                                   
         SPACE 3                                                                
DREC     NTR1                                                                   
         LA    R2,PROWGRNH                                                      
         GOTO1 NAMEOUT                                                          
         GOTO1 PERSOUT                                                          
         MVC   PROLACT,SPACES                                                   
         MVC   PROLACT(14),=C'LAST ACTIVITY:'                                   
         MVC   PROLACT+15(20),WORK+20                                           
         OI    PROLACTH+6,X'80'                                                 
         B     XIT                                                              
         EJECT                                                                  
*              PRINT WORK GROUP REPORT                                          
         SPACE 2                                                                
PREP     NTR1                                                                   
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
*                                                                               
         GOTO1 SORTER,DMCB,SORTFLD,RECTYPE,0                                    
         LA    R4,KEY                                                           
         USING ACWGKEY,R4                                                       
         XC    ACWGKEY,ACWGKEY                                                  
         MVI   ACWGRTYP,ACWGEQU                                                 
         MVI   ACWGSREC,ACWGSEQU                                                
         MVC   ACWGCUL,CUL                                                      
         GOTO1 HIGH                                                             
         B     PREP4                                                            
         SPACE 1                                                                
PREP2    GOTO1 SEQ                                                              
         SPACE 1                                                                
PREP4    CLC   ACWGKEY(ACWGCODE-ACWGKEY),KEYSAVE   CHECK C/B                    
         BNE   PREP6                                                            
*                                                                               
         XC    SORTREC,SORTREC                                                  
         LA    R3,SORTREC                                                       
         USING SORTD,R3                                                         
         LA    RE,SORTRECL                                                      
         STH   RE,SORTRLEN                                                      
         MVC   SORTWG,ACWGCODE                                                  
         GOTO1 SETNAME,DMCB,AIO,WORK                                            
         MVC   SORTNAME,WORK                                                    
         GOTO1 SORTER,DMCB,=C'PUT',SORTD                                        
         B     PREP2                                                            
*                                                                               
PREP6    MVC   KEY,SPACES          READ WORK CODE RECORDS                       
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(3),CUL                                                     
         GOTO1 HIGH                                                             
         B     PREP8                                                            
         SPACE 1                                                                
PREP7    GOTO1 SEQ                                                              
         SPACE 1                                                                
PREP8    CLC   KEY(4),KEYSAVE      CHECK C/B                                    
         BNE   PREP10                                                           
         SPACE 1                                                                
         MVI   ELCODE,ACANELQ      GET WORK CODE ELEMENT                        
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACANALD,R6                                                       
         CLI   ACANGRUP,0          TEST IF ASSIGNED TO GROUP                    
         BE    PREP7               NO-SKIP OVER                                 
         XC    SORTREC,SORTREC                                                  
         LA    R3,SORTREC                                                       
         LA    RE,SORTRECL                                                      
         STH   RE,SORTRLEN                                                      
         MVC   SORTWG,ACANGRUP      SET WORK CODE GROUP                         
         MVC   SORTWORK,ACANCODE    EXTRACT WORK CODE                           
         MVC   SORTNAME(L'ACANDESC),ACANDESC  AND DESCRIPTION                   
         GOTO1 SORTER,DMCB,=C'PUT',SORTD                                        
         B     PREP7                                                            
*                                                                               
PREP10   MVI   PENDSW,C'N'         SET PENDING SWITCH TO NO                     
*                                                                               
PREP12   GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R3,15,DMCB+4                                                     
         BZ    PREP15                                                           
         LA    R2,P                                                             
         USING PRTD,R2                                                          
         OC    SORTWORK,SORTWORK   TEST FOR WORK CODE RECORD                    
         BNZ   PREP14              YES                                          
         CLI   PENDSW,C'Y'         TEST FOR PENDING WORK GROUP                  
         BNE   PREP13                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PREP13   GOTO1 SPOOL,DMCB,(R8)     SKIP A LINE BEFORE                           
         MVC   PRTWG,SORTWG        EXTRACT WORK GROUP                           
         MVC   PRTNAME,SORTNAME    AND WORK GROUP NAME                          
         MVI   PENDSW,C'Y'         SET PENDING WORK GROUP LINE                  
         B     PREP12              GET NEXT RECORD                              
*                                                                               
PREP14   MVC   PRTWORK,SORTWORK    EXTRACT WORK CODE                            
         MVC   PRTDESC,SORTNAME    AND DESCRIPTION                              
         CLI   PENDSW,C'Y'         TEST PENDING WORK GROUP                      
         BNE   *+12                                                             
         MVI   ALLOWLIN,2          ALLOW FOR 2 WORK CODE LINES                  
         MVI   PENDSW,C'N'         TURN SWITCH OFF                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PREP12              GET NEXT SORT RECORD                         
*                                                                               
PREP15   CLI   PENDSW,C'Y'         TEST FOR PENDING GROUP LINE                  
         BNE   PREP18                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PREP18   GOTO1 SORTER,DMCB,=C'END'                                              
*                                                                               
PREPX    B     XIT                                                              
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
SORTFLD  DC    CL80'SORT FIELDS=(5,3,A),FORMAT=BI,WORK=1'                       
RECTYPE  DC    CL80'RECORD TYPE=V,LENGTH=100'                                   
         SPACE 2                                                                
MYSPECS  DS    0F                                                               
         SSPEC H1,2,CREATED        SPECS FOR REGULAR PRINTING                   
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,45,C'WORK GROUP LISTING'                                      
         SSPEC H2,45,C'------------------'                                      
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,REPORT                                                     
         SSPEC H4,110,PAGE                                                      
*                                                                               
         SSPEC H8,2,C'WORK'                                                     
         SSPEC H9,2,C'GROUP'                                                    
         SSPEC H8,10,C'WORK GROUP NAME'                                         
         SSPEC H8,52,C'WORK'                                                    
         SSPEC H9,52,C'CODE'                                                    
         SSPEC H8,60,C'DESCRIPTION'                                             
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECTS ARE HIDDEN IN HERE                                        
         SPACE 3                                                                
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROFDD                                                       
         SPACE 2                                                                
         DS    0F                                                               
SORTREC  DS    CL(SORTRECL)                                                     
PENDSW   DS    C                                                                
POINTERS DS    XL(8*54+1)                                                       
         EJECT                                                                  
* DSECT TO COVER SORT RECORD                                                    
*                                                                               
SORTD    DSECT                                                                  
SORTRLEN DS    H                                                                
         DS    H                                                                
SORTKEY  DS    0CL3                                                             
SORTWG   DS    C                   WORK GROUP                                   
SORTWORK DS    CL2                 WORK CODE                                    
SORTDATA DS    0CL36                                                            
SORTNAME DS    CL36                                                             
SORTRECL EQU   *-SORTD                                                          
         SPACE 2                                                                
* DSECT TO COVER PRINT LINE                                                     
*                                                                               
PRTD     DSECT                                                                  
PRTLBOX  DS    C                   LEFT HAND BOX                                
PRTWG    DS    C                   WORK GROUP                                   
         DS    CL6                                                              
PRTBOX1  DS    C                                                                
PRTNAME  DS    CL36                WORK GROUP NAME                              
         DS    CL5                                                              
PRTBOX2  DS    C                                                                
PRTWORK  DS    CL2                 WORKCODE                                     
         DS    CL5                                                              
PRTBOX3  DS    C                                                                
PRTDESC  DS    CL15                WORKCODE DESCRIPTION                         
         DS    C                                                                
PRTRBOX  DS    C                                                                
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACPRO0D   09/12/02'                                      
         END                                                                    
