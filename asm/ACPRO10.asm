*          DATA SET ACPRO10    AT LEVEL 001 AS OF 01/28/93                      
*PHASE T60B10A,*                                                                
         TITLE 'T60B10 - PRICE LIST'                                            
T60B10   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B10**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         CLI   MODE,LISTRECS                                                    
         BNE   XIT                                                              
         EJECT                                                                  
*              HANDLE I/O FOR PRICES                                            
*                                                                               
         LA    R4,KEY                                                           
         USING PRCRECD,R4                                                       
         CLI   PRCKTYP,PRCKTYPQ    ARE WE HANDLING PRICES YET?                  
         BNE   *+12                NO                                           
         CLI   PRCKSUB,PRCKSUBQ                                                 
         BE    LIST2               YES                                          
*                                                                               
         LA    R4,USERKEY                                                       
         XC    EFFDATE,EFFDATE                                                  
         XC    PRCKEY,PRCKEY                                                    
         MVI   PRCKTYP,PRCKTYPQ                                                 
         MVI   PRCKSUB,PRCKSUBQ                                                 
         MVC   PRCKCPY(3),CUL                                                   
*                                                                               
         LA    R2,PRCOFGH          OPTIONAL OFFICE GROUP FILTER                 
         CLI   5(R2),0                                                          
         BE    VKEY2                                                            
         GOTO1 VALOG                                                            
*                                                                               
VKEY2    MVC   PRCKOFG,PRCOFG                                                   
         LA    R2,PRCOFFH          OPTIONAL OFFICE FILTER                       
         CLI   5(R2),0                                                          
         BE    VKEY4                                                            
         MVI   ERROR,NOTOFNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   PRCOFGH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALOFF                                                           
*                                                                               
VKEY4    MVC   PRCKOFC,PRCOFF                                                   
         LA    R2,PRCCLIH          OPTIONAL CLIENT FILTER                       
         CLI   5(R2),0                                                          
         BE    VKEY6                                                            
         MVI   ERROR,NOTCLNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   PRCOFGH+5,0                                                      
         BNE   ERREND                                                           
         MVI   ERROR,NOTCLNOF      NOT COMPATIBLE WITH OFFICE                   
         CLI   PRCOFFH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALCLI                                                           
*                                                                               
VKEY6    MVC   PRCKCLI,PRCCLI                                                   
         LA    R2,PRCPROH          OPTIONAL PRODUCT FILTER                      
         CLI   5(R2),0                                                          
         BE    VKEY8                                                            
         MVI   ERROR,NEEDCLI       IF INPUT, NEED CLIENT AS WELL                
         CLI   PRCCLIH+5,0                                                      
         BE    ERREND                                                           
         GOTO1 VALPROD                                                          
*                                                                               
VKEY8    MVC   PRCKPRO,PRCPRO                                                   
         LA    R2,PRCJOBH          OPTIONAL JOB FILTER                          
         CLI   5(R2),0                                                          
         BE    VKEY10                                                           
         MVI   ERROR,NEEDPRO       IF INPUT, NEED PRODUCT AS WELL               
         CLI   PRCPROH+5,0                                                      
         BE    ERREND                                                           
         GOTO1 VALJOB                                                           
*                                                                               
VKEY10   MVC   PRCKJOB,PRCJOB                                                   
         LA    R2,PRCMGRH          OPTIONAL MEDIA GROUP FILTER                  
         CLI   5(R2),0                                                          
         BE    VKEY12                                                           
         MVI   ERROR,NOTJBNME      NOT COMPATIBLE WITH JOB                      
         CLI   PRCJOBH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALMG                                                            
*                                                                               
VKEY12   MVC   PRCKMGR,PRCMGR                                                   
         LA    R2,PRCMEDH          OPTIONAL MEDIA FILTER                        
         CLI   5(R2),0                                                          
         BE    VKEY14                                                           
         MVI   ERROR,NOTMENMG      NOT COMPATIBLE WITH MEDIA GROUP              
         CLI   PRCMGRH+5,0                                                      
         BNE   ERREND                                                           
         MVI   ERROR,NOTJBNME      NOT COMPATIBLE WITH JOB                      
         CLI   PRCJOBH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALMED                                                           
*                                                                               
VKEY14   MVC   PRCKMED,PRCMED                                                   
         LA    R2,PRCWGRH          OPTIONAL WORK GROUP FILTER                   
         CLI   5(R2),0                                                          
         BE    VKEY16                                                           
         GOTO1 VALWG                                                            
*                                                                               
VKEY16   MVC   PRCKWGR,PRCWGR                                                   
         LA    R2,PRCWRKH          OPTIONAL WORKCODE FILTER                     
         CLI   5(R2),0                                                          
         BE    VKEY18                                                           
         MVI   ERROR,NOTWKNWG      NOT COMPATIBLE WITH WORK GROUP               
         CLI   PRCWGRH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALWORK                                                          
*                                                                               
VKEY18   MVC   PRCKWRK,PRCWRK                                                   
         LA    R2,PRCEFFH          OPTIONAL DATE FILTER                         
         CLI   5(R2),0                                                          
         BE    VKEY20                                                           
         GOTO1 ANY                                                              
         MVI   ERROR,INVDATE                                                    
         GOTO1 DATVAL,DMCB,WORK,DUB                                             
         OC    DMCB,DMCB                                                        
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,DUB,(1,EFFDATE)                                      
         XC    EFFDATE,EFFS                                                     
         MVC   PRCKEFF,EFFDATE                                                  
*                                                                               
VKEY20   MVC   KEY,USERKEY                                                      
*                                                                               
LIST2    GOTO1 HIGH                                                             
         B     LIST6                                                            
*                                                                               
LIST4    GOTO1 SEQ                                                              
*                                                                               
         USING LSTDSECT,R3                                                      
LIST6    L     R4,AIO                                                           
         LA    R3,LISTAR                                                        
         CLC   KEY(3),KEYSAVE      RECORD/AGENCY CHANGE?                        
         BNE   XIT                 YES                                          
         MVC   LISTAR,SPACES                                                    
         CLI   PRCOFGH+5,0         CHECK FILTERS                                
         BE    LIST8                                                            
         CLC   PRCKOFG,PRCOFG                                                   
         BNE   LIST4                                                            
*                                                                               
LIST8    MVC   LSTOFG,PRCKOFG                                                   
         CLI   PRCOFFH+5,0                                                      
         BE    LIST10                                                           
         LA    R2,PRCOFFH                                                       
         GOTO1 ANY                                                              
         CLC   PRCKOFC,WORK                                                     
         BNE   LIST4                                                            
*                                                                               
LIST10   MVC   LSTOFF,PRCKOFC                                                   
         CLI   PRCCLIH+5,0                                                      
         BE    LIST12                                                           
         LA    R2,PRCCLIH                                                       
         GOTO1 ANY                                                              
         CLC   PRCKCLI,WORK                                                     
         BNE   LIST4                                                            
*                                                                               
LIST12   MVC   LSTCLI,PRCKCLI                                                   
         CLI   PRCPROH+5,0                                                      
         BE    LIST14                                                           
         LA    R2,PRCPROH                                                       
         GOTO1 ANY                                                              
         CLC   PRCKPRO,WORK                                                     
         BNE   LIST4                                                            
*                                                                               
LIST14   MVC   LSTPRO,PRCKPRO                                                   
         CLI   PRCJOBH+5,0                                                      
         BE    LIST16                                                           
         LA    R2,PRCJOBH                                                       
         GOTO1 ANY                                                              
         CLC   PRCKJOB,WORK                                                     
         BNE   LIST4                                                            
*                                                                               
LIST16   MVC   LSTJOB,PRCKJOB                                                   
         CLI   PRCMGRH+5,0                                                      
         BE    LIST18                                                           
         CLC   PRCKMGR,PRCMGR                                                   
         BNE   LIST4                                                            
*                                                                               
LIST18   MVC   LSTMGR,PRCKMGR                                                   
         CLI   PRCMEDH+5,0                                                      
         BE    LIST20                                                           
         CLC   PRCKMED,PRCMED                                                   
         BNE   LIST4                                                            
*                                                                               
LIST20   MVC   LSTMED,PRCKMED                                                   
         CLI   PRCWGRH+5,0                                                      
         BE    LIST22                                                           
         CLC   PRCKWGR,PRCWGR                                                   
         BNE   LIST4                                                            
*                                                                               
LIST22   MVC   LSTWGR,PRCKWGR                                                   
         CLI   PRCWRKH+5,0                                                      
         BE    LIST24                                                           
         CLC   PRCKWRK,PRCWRK                                                   
         BNE   LIST4                                                            
*                                                                               
LIST24   MVC   LSTWRK,PRCKWRK                                                   
         CLI   PRCEFFH+5,0                                                      
         BE    LIST26                                                           
         LA    R2,PRCEFFH                                                       
         GOTO1 ANY                                                              
         MVI   ERROR,INVDATE                                                    
         GOTO1 DATVAL,DMCB,WORK,DUB                                             
         OC    DMCB,DMCB                                                        
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,DUB,(1,EFFDATE)                                      
         XC    EFFDATE,EFFS                                                     
         CLC   PRCKEFF,EFFDATE                                                  
         BNE   LIST4                                                            
*                                                                               
LIST26   MVC   EFFDATE,PRCKEFF                                                  
         XC    EFFDATE,EFFS                                                     
         GOTO1 DATCON,DMCB,(1,EFFDATE),(8,LSTEFF)                               
         GOTO1 LISTMON                                                          
         B     LIST4                                                            
         EJECT                                                                  
EFFS     DC    8X'FF'                                                           
*                                                                               
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
ERREND   GOTO1 VERRCUR                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
LSTDSECT DSECT                                                                  
         DS    CL2                                                              
LSTOFG   DS    CL1                                                              
         DS    CL4                                                              
LSTOFF   DS    CL2                                                              
         DS    CL4                                                              
LSTCLI   DS    CL3                                                              
         DS    CL6                                                              
LSTPRO   DS    CL3                                                              
         DS    CL7                                                              
LSTJOB   DS    CL6                                                              
         DS    CL4                                                              
LSTMGR   DS    CL1                                                              
         DS    CL5                                                              
LSTMED   DS    CL1                                                              
         DS    CL5                                                              
LSTWGR   DS    CL1                                                              
         DS    CL4                                                              
LSTWRK   DS    CL2                                                              
         DS    CL4                                                              
LSTEFF   DS    CL8                                                              
         EJECT                                                                  
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROE0D                                                       
TODAY    DS    CL3                                                              
EFFDATE  DS    PL3                                                              
USERKEY  DS    CL48                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACPRO10   01/28/93'                                      
         END                                                                    
