*          DATA SET ACPRO29    AT LEVEL 002 AS OF 09/12/02                      
*PHASE T60B29A,*                                                                
         TITLE 'T60B29 - COMMENT REPORT'                                        
T60B29   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B29**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              HANDLE I/O FOR COMMENT RECORDS                                   
         SPACE 3                                                                
PREP     LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'                                                        
         MVC   KEY+1(1),CUL                                                     
         GOTO1 HIGH                                                             
         B     LIST4                                                            
         SPACE 1                                                                
LIST2    GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST4    CLC   KEY(2),KEYSAVE      CHECK C/B                                    
         BNE   XIT                                                              
         LA    R3,P                                                             
         L     R4,AIO                                                           
         MVC   1(6,R3),2(R4)       SHOW COMMENT CODE                            
         SPACE 1                                                                
         MVI   ELCODE,X'A1'        ACTIVITY                                     
         BAS   RE,GETELIO                                                       
         BNE   LIST6                                                            
         USING ACPID,R6                                                         
         MVC   40(8,R3),ACPIPERS   PERSON & DATE                                
         GOTO1 DATCON,DMCB,(1,ACPIDATE),(8,49(R3))                              
         SPACE 1                                                                
LIST6    MVI   ELCODE,X'A3'                                                     
         BAS   RE,GETELIO                                                       
         BNE   LIST8                                                            
         USING ACCID,R6                                                         
         MVC   33(4,R3),ACCIFILT   COMMENT FILTER                               
         MVC   9(15,R3),ACCIDESC          AND DESCRIPTION                       
         SPACE 1                                                                
LIST8    CLI   PROFILTH+5,0        WAS A COMMENT FILTER REQUESTED               
         BE    LIST14                                                           
         LA    RE,PROFILT                                                       
         LA    RF,33(R3)                                                        
         ZIC   R0,PROFILTH+5                                                    
         SPACE 1                                                                
LIST10   CLI   0(RE),C'*'          UNLESS COLUMN IS 'WILD'                      
         BE    LIST12                                                           
         CLI   0(RE),X'41'                                                      
         BL    LIST12                                                           
         CLC   0(1,RE),0(RF)       CHECK MATCH AGAINST RECORD FILTER            
         BNE   LIST2                                                            
         SPACE 1                                                                
LIST12   LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,LIST10                                                        
         SPACE 1                                                                
LIST14   BAS   RE,PROCCOM                                                       
         B     LIST2                                                            
         EJECT                                                                  
*              PROCESS A COMMENT RECORD                                         
         SPACE 3                                                                
PROCCOM  NTR1                                                                   
         MVI   ELCODE,X'3E'                                                     
         BAS   RE,GETELIO                                                       
         LA    R2,1                FIRST COUNT LINES IN R2                      
         B     PC4                                                              
         SPACE 1                                                                
PC2      BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
PC4      BNE   PC6                                                              
         LA    R2,1(R2)                                                         
         B     PC2                                                              
         SPACE 1                                                                
PC6      CH    R2,=H'13'                                                        
         BL    *+8                                                              
         LA    R2,13                                                            
         STC   R2,ALLOWLIN         AND ENSURE CHUNK STAYS TOGETHER              
         BCTR  R2,0                                                             
         BAS   RE,GETELIO                                                       
         B     PC14                                                             
         SPACE 1                                                                
PC12     BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
PC14     BNE   PC16                                                             
         USING ACOMMD,R6                                                        
         ZIC   R1,ACOMLEN                                                       
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   58(0,R3),ACOMMENT                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BCT   R2,PC12                                                          
         SPACE 1                                                                
PC16     GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              HEAD HOOK                                                        
         SPACE 3                                                                
HOOK     NTR1                                                                   
         LA    R3,H1+48            DEAL WITH HEADING                            
         MVC   0(32,R3),=CL32'PRODUCTION COMMENT REPORT'                        
         GOTO1 CENTER,DMCB,(R3),32                                              
         GOTO1 UNDERLIN,DMCB,(32,(R3)),(X'BF',132(R3))                          
         SPACE 1                                                                
         MVC   H8+1(7),=C'COMMENT'                                              
         MVC   H9+1(6),=C'NUMBER'                                               
         MVC   H8+9(11),=C'DESCRIPTION'                                         
         MVC   H8+32(7),=C'FILTERS'                                             
         MVC   H8+41(13),=C'LAST ACTIVITY'                                      
         MVC   H9+41(13),=C' BY       ON '                                      
         MVC   H8+58(12),=C'COMMENT TEXT'                                       
         SPACE 1                                                                
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
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+8,C'C'                                                   
         MVI   BOXCOLS+31,C'C'                                                  
         MVI   BOXCOLS+39,C'C'                                                  
         MVI   BOXCOLS+57,C'C'                                                  
         MVI   BOXCOLS+130,C'R'                                                 
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
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,REPORT                                                     
         SSPEC H4,110,PAGE                                                      
         DC    X'00'                                                            
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
*DDBIGBOX                                                                       
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROD9D                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACPRO29   09/12/02'                                      
         END                                                                    
