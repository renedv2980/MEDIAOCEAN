*          DATA SET ACPRO28    AT LEVEL 008 AS OF 09/12/02                      
*PHASE T60B28A,*                                                                
         TITLE 'T60B28 - MEDIA REPORT'                                          
T60B28   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B28**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE FOR KEY FIELD VALIDATIONS                                         
*                                                                               
VKEY     NTR1                                                                   
         MVI   FILTMG,0                                                         
         LA    R2,PROMGRH                                                       
         XC    PROMGRN,PROMGRN                                                  
         OI    PROMGRNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VKEYX                                                            
         MVI   OPTION,C'Y'                                                      
         GOTO1 VALMG                                                            
         MVC   FILTMG,8(R2)                                                     
         SPACE 1                                                                
VKEYX    B     XIT                                                              
         EJECT                                                                  
*              HANDLE I/O FOR MEDIA RECORDS                                     
         SPACE 3                                                                
PREP     NTR1                                                                   
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'09'                                                        
         MVC   KEY+1(1),CUL                                                     
         GOTO1 HIGH                                                             
         B     PREP4                                                            
         SPACE 1                                                                
PREP2    GOTO1 SEQ                                                              
         SPACE 1                                                                
PREP4    CLC   KEY(2),KEYSAVE      CHECK C/B                                    
         BNE   XIT                                                              
         LA    R3,P                                                             
         USING PRTD,R3                                                          
         L     R4,AIO                                                           
         SPACE 1                                                                
         MVI   ELCODE,ACMDELQ      GET MEDIA ELEMENT                            
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACMEDIAD,R6                                                      
         CLI   FILTMG,0            TEST FOR MEDIA GROUP FILTER                  
         BE    *+14                                                             
         CLC   FILTMG,ACMDGRP      TEST FOR MATCH ON FILTER                     
         BNE   PREP2               NO                                           
         SPACE 1                                                                
         MVC   PRTMED,ACMDCODE                                                  
         MVC   PRTDESC,ACMDDESC                                                 
         MVC   PRTCOMM(14),ACMDCOMM+1                                           
         MVC   PRTMGR,ACMDGRP                                                   
         MVC   PRTBILL,ACMDRSET                                                 
         MVC   PRTANAL,ACMDANAL                                                 
         MVI   PRTBILLT,C'Y'                                                    
         TM    ACMDSTAT,X'80'      TEST PRINT MEDIA NAME IN HEADLINE            
         BO    *+8                                                              
         MVI   PRTBILLT,C'N'                                                    
         SPACE 1                                                                
         MVC   HALF(1),ACMDCODE                                                 
         MVI   HALF+1,ACMNEST                                                   
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('ACMNELQ',AIO),(2,HALF),0              
         CLI   12(R1),0                                                         
         BNE   PREP6                                                            
         L     R6,12(R1)                                                        
         USING ACMND,R6                                                         
         ZIC   R2,ACMNLEN                                                       
         SH    R2,=Y(ACMNAME-ACMND)                                             
         GOTO1 CHOPPER,DMCB,((R2),ACMNAME),(L'PRTENAM,PRTENAM),(C'P',2)X        
               ,0                                                               
         SPACE 1                                                                
PREP6    MVI   ALLOWLIN,3          ALLOW FOR SKIPPED LINE                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PREP2                                                            
         EJECT                                                                  
*              HEAD HOOK                                                        
         SPACE 3                                                                
HOOK     NTR1                                                                   
         CLI   FILTMG,0            TEST FOR MEDIA GROUP FILTER                  
         BE    HOOK2                                                            
         MVC   H4+1(11),=C'MEDIA GROUP'                                         
         MVC   H4+14(1),FILTMG                                                  
         MVC   H4+16(L'PROMGRN),PROMGRN                                         
         SPACE 1                                                                
HOOK2    L     R4,ABOX                                                          
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
         MVI   PRTBOX4-PRTD(R2),C'C'                                            
         MVI   PRTBOX5-PRTD(R2),C'C'                                            
         MVI   PRTBOX6-PRTD(R2),C'C'                                            
         MVI   PRTRBOX-PRTD(R2),C'R'                                            
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              SPECS FOR HEADINGS ETC                                           
         SPACE 3                                                                
MYSPECS  DS    0F                                                               
         SSPEC H1,2,CREATED        SPECS FOR REGULAR PRINTING                   
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,48,C'MEDIA LISTING'                                           
         SSPEC H2,48,C'-------------'                                           
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,REPORT                                                     
         SSPEC H4,110,PAGE                                                      
         SSPEC H8,2,C'-------MEDIA--------'                                     
         SSPEC H9,2,C'CODE'                                                     
         SSPEC H9,7,C'DESCRIPTION'                                              
         SSPEC H8,24,C'COMMISSION A/C'                                          
         SSPEC H8,41,C'MEDIA GROUP'                                             
         SSPEC H8,54,C'RESET BILL'                                              
         SSPEC H9,54,C'  NUMBER'                                                
         SSPEC H8,67,C'UNIT FOR'                                                
         SSPEC H9,67,C'ANALYSIS'                                                
         SSPEC H8,77,C'BILL TITLE'                                              
         SSPEC H9,77,C'OPTION'                                                  
         SSPEC H8,90,C'ESTIMATE NAME'                                           
         DC    X'00'                                                            
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
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
       ++INCLUDE ACPROD8D                                                       
         SPACE 2                                                                
FILTMG   DS    C                                                                
         SPACE 2                                                                
* DSECT TO COVER PRINT LINES                                                    
*                                                                               
PRTD     DSECT                                                                  
PRTLBOX  DS    C                                                                
PRTMED   DS    C                   MEDIA CODE                                   
         DS    CL4                                                              
PRTDESC  DS    CL15                DESCRIPTION                                  
         DS    C                                                                
PRTBOX1  DS    C                                                                
PRTCOMM  DS    CL15                COMMISSION A/C                               
         DS    C                                                                
PRTBOX2  DS    C                                                                
PRTMGR   DS    C                   MEDIA GROUP                                  
         DS    CL10                SPARE                                        
         DS    C                                                                
PRTBOX3  DS    C                                                                
PRTBILL  DS    CL4                 LAST BILL NUMBER                             
         DS    CL6                                                              
         DS    CL2                 SPARE                                        
PRTBOX4  DS    C                                                                
PRTANAL  DS    C                   UNIT FOR ANALYSIS                            
         DS    CL7                                                              
         DS    C                                                                
PRTBOX5  DS    C                                                                
PRTBILLT DS    C                   BILL TITLE OPTION                            
         DS    CL10                                                             
         DS    C                                                                
PRTBOX6  DS    C                                                                
PRTENAM  DS    CL20                ESTIMATE NAME                                
PRTRBOX  DS    C                                                                
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACPRO28   09/12/02'                                      
         END                                                                    
