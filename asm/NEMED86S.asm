*          DATA SET NEMED86S   AT LEVEL 046 AS OF 05/01/02                      
*PHASE T31E86A                                                                  
         TITLE 'T31E86 - UNIVERSE LIST REPORT'                                  
T31E86   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NEUN**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1                                                       
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         EJECT                                                                  
         SPACE 3                                                                
         CLI   MODE,VALKEY                                                      
         BE    VK                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY *                                                                
         SPACE                                                                  
VK       DS    0H                                                               
         LA    R2,SPLTYPH                                                       
*                                                                               
         LA    R2,SPLCOMH                                                       
         CLI   SPLCOM,C'Y'                                                      
         BE    VKEXIT                                                           
         CLI   SPLCOM,C'N'                                                      
         BE    VKEXIT                                                           
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
         SPACE                                                                  
VKEXIT   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         SPACE                                                                  
* LIST RECORDS *                                                                
LR       DS    0H                                                               
*                                                                               
         NETGO NVSETSPT,DMCB                                                    
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVC   SYSFIL,=C'SPTFIL  '                                              
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D22'                                                  
         MVC   KEY+2(2),NBSELAGY                                                
         GOTO1 HIGH                                                             
         B     LR5                                                              
LRSEQ    DS    0H                                                               
         GOTO1 SEQ                                                              
         SPACE                                                                  
LR5      CLC   KEY(4),KEYSAVE                                                   
         BNE   LRX                                                              
         GOTO1 GETREC                                                           
         L     R6,NBAIO                                                         
         USING NUNEL02,R6                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   SPLTYP,X'40'                                                     
         BNH   LR10                                                             
         CLC   NUNTYPE,SPLTYP                                                   
         BNE   LRSEQ                                                            
         MVC   TYPESV,NUNTYPE                                                   
*                                                                               
LR10     DS    0H                                                               
         L     R2,NBAIO                                                         
         CLC   CDS,10(R2)          IS IT SAME CODE AS PREV                      
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   CDS,10(R2)                                                       
         CLI   10(R2),1            IS IT UNIV CODE/END DATE                     
         BNE   LR11                                                             
         XC    DTCDSV,DTCDSV                                                    
         UNPK  WORK+20(5),11(3,R2)    PWOS                                      
         LA    R4,WORK+20                                                       
         EDIT  (C4,0(R4)),(6,DTCDSV),ALIGN=LEFT                                 
         MVC   DTCDSV+4(2),=F'0'                                                
         B     LR12                                                             
LR11     GOTO1 DATCON,DMCB,(2,11(R2)),(0,DTCDSV)                                
*                                                                               
LR12     LA    R4,NUNIVES                                                       
         LA    R2,OTHERS+4                                                      
         BAS   RE,ADDIT                                                         
         LA    R4,4(R4)                                                         
         LA    R2,TBLWMN+4                                                      
         BAS   RE,ADDIT                                                         
         LA    R2,ALLTOT+4                                                      
         BAS   RE,ADDIT                                                         
         LA    R4,4(R4)                                                         
         LA    R2,TBLWMN+8                                                      
         BAS   RE,ADDIT                                                         
         LA    R2,ALLTOT+8                                                      
         BAS   RE,ADDIT                                                         
         LA    R4,4(R4)                                                         
         LA    R2,TBLWMN+12                                                     
         BAS   RE,ADDIT                                                         
         LA    R2,ALLTOT+12                                                     
         BAS   RE,ADDIT                                                         
         LA    R4,8(R4)            SKIP WMN55-64                                
         LA    R2,TBLWMN+20                                                     
         BAS   RE,ADDIT                                                         
         LA    R2,ALLTOT+20                                                     
         BAS   RE,ADDIT                                                         
         LA    R4,4(R4)                                                         
         LA    R2,TBLWMN                                                        
         BAS   RE,ADDIT                                                         
         LA    R2,ALLTOT                                                        
         BAS   RE,ADDIT                                                         
*                                                                               
         LA    R4,4(R4)                                                         
         LA    R2,TBLMEN+4                                                      
         BAS   RE,ADDIT                                                         
         LA    R2,ALLTOT+4                                                      
         BAS   RE,ADDIT                                                         
         LA    R4,4(R4)                                                         
         LA    R2,TBLMEN+8                                                      
         BAS   RE,ADDIT                                                         
         LA    R2,ALLTOT+8                                                      
         BAS   RE,ADDIT                                                         
         LA    R4,4(R4)                                                         
         LA    R2,TBLMEN+12                                                     
         BAS   RE,ADDIT                                                         
         LA    R2,ALLTOT+12                                                     
         BAS   RE,ADDIT                                                         
         LA    R4,8(R4)           SKIP MEN55-64                                 
         LA    R2,TBLMEN+20                                                     
         BAS   RE,ADDIT                                                         
         LA    R2,ALLTOT+20                                                     
         BAS   RE,ADDIT                                                         
         LA    R4,4(R4)                                                         
         LA    R2,TBLMEN                                                        
         BAS   RE,ADDIT                                                         
         LA    R2,ALLTOT                                                        
         BAS   RE,ADDIT                                                         
*                                                                               
         LA    R4,4(R4)                                                         
         LA    R2,OTHERS+8                                                      
         BAS   RE,ADDIT                                                         
         LA    R4,4(R4)                                                         
         LA    R2,4(R2)                                                         
         BAS   RE,ADDIT                                                         
         LA    R4,12(R4)                                                        
         LA    R2,OTHERS+16                                                     
         BAS   RE,ADDIT                                                         
         LA    R4,NUNIVES+64       TN GIRLS                                     
         LA    R2,OTHERS+20                                                     
         BAS   RE,ADDIT                                                         
         LA    R4,NUNIVES+76       CH6-11                                       
         LA    R2,OTHERS+28                                                     
         BAS   RE,ADDIT                                                         
         LA    R4,4(R4)            CHILD                                        
         LA    R2,OTHERS+24                                                     
         BAS   RE,ADDIT                                                         
         LA    R4,NUNIVES+84       HOMES                                        
         LA    R2,OTHERS                                                        
         BAS   RE,ADDIT                                                         
*                                                                               
         LA    R4,NUNIVES+88       WMN35-64                                     
         LA    R2,TBLWMN+16                                                     
         BAS   RE,ADDIT                                                         
         LA    R2,ALLTOT+16                                                     
         BAS   RE,ADDIT                                                         
         LA    R4,4(R4)            MN35-64                                      
         LA    R2,TBLMEN+16                                                     
         BAS   RE,ADDIT                                                         
         LA    R2,ALLTOT+16                                                     
         BAS   RE,ADDIT                                                         
*                                                                               
         MVI   SPACING,1                                                        
         BAS   RE,PRNT                                                          
         BAS   RE,CLEARTBL                                                      
*                                                                               
         MVI   ELCODE,X'C1'        USER DEMOS                                   
         USING UDEUND,R6                                                        
         L     R6,NBAIO                                                         
         BAS   RE,GETEL                                                         
         BNE   LR25                                                             
*                                                                               
         LA    R2,P+35                                                          
LR15     MVC   WORK(8),SPACES                                                   
         MVC   WORK(7),UDEUNNAM                                                 
         SR    R3,R3                                                            
         LA    R5,WORK                                                          
LR15B    CLI   0(R5),X'40'                                                      
         BNH   LR15D                                                            
         LA    R5,1(R5)                                                         
         LA    R3,1(R3)                                                         
         B     LR15B                                                            
LR15D    LR    R1,R3                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),WORK                                                     
         AR    R2,R3                                                            
         MVI   0(R2),C'='                                                       
         LA    R3,UDEUNIV                                                       
         LA    R2,1(R2)                                                         
         LA    R4,1                                                             
         BAS   RE,EDITIT                                                        
         BAS   RE,NEXTEL                                                        
         BNE   LR25                                                             
         LA    R2,7(R2)                                                         
         B     LR15                                                             
LR25     DS    0H                                                               
         CLI   SPLCOM,C'Y'                                                      
         BNE   LR27                                                             
         L     R6,NBAIO           COMMENTS                                      
         MVI   ELCODE,X'66'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR27                                                             
         ZIC   R5,1(R6)                                                         
         SH    R5,=H'3'                                                         
         BM    LR27                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P2+35(0),2(R6)                                                   
*                                                                               
LR27     MVI   SPACING,3                                                        
         BAS   RE,BOXRESET                                                      
         BAS   RE,PRINTIT                                                       
         B     LRSEQ                                                            
LRX      B     EXIT                                                             
         SPACE 2                                                                
CLEARTBL NTR1                                                                   
         LA    R1,TABLE                                                         
         LA    R2,26                                                            
XCIT     XC    0(4,R1),0(R1)                                                    
         LA    R1,4(R1)                                                         
         BCT   R2,XCIT                                                          
         B     EXIT                                                             
         EJECT                                                                  
PRNT     NTR1                                                                   
         MVC   P+21(6),DTCDSV                                                   
         CLI   DTCDSV+5,X'40'      IF YYMMDD                                    
         BNH   PRNT5                                                            
         MVC   P+21(2),DTCDSV+2    CHANGE TO MM/DD/YY                           
         MVI   P+23,C'/'                                                        
         MVC   P+24(2),DTCDSV+4                                                 
         MVI   P+26,C'/'                                                        
         MVC   P+27(2),DTCDSV                                                   
PRNT5    MVC   P+30(1),TYPESV                                                   
         MVC   P+35(5),=C'WOMEN'                                                
         MVC   P2+35(3),=C'MEN'                                                 
         MVC   P3+35(6),=C'ADULTS'                                              
         LA    R2,P+45                                                          
         LA    R3,TABLE                                                         
         LA    R4,6                                                             
         BAS   RE,EDITIT                                                        
         LA    R2,P2+45                                                         
         LA    R3,TBLMEN                                                        
         BAS   RE,EDITIT                                                        
         LA    R2,P3+45                                                         
         LA    R3,ALLTOT                                                        
         BAS   RE,EDITIT                                                        
         MVI   ALLOWLIN,8                                                       
         BAS   RE,PRINTIT                                                       
         MVI   ALLOWLIN,0                                                       
         BAS   RE,PRINTIT          SKIP LINE                                    
         MVC   P+35(5),=C'HOMES'                                                
         MVC   P+46(5),=C'ALL+2'                                                
         MVC   P+58(3),=C'LOH'                                                  
         MVC   P+66(5),=C'WWORK'                                                
         MVC   P+76(5),=C'TEENS'                                                
         MVC   P+84(7),=C'FM12-17'                                              
         MVC   P+96(5),=C'CHILD'                                                
         MVC   P+105(6),=C'CH6-11'                                              
         LA    R4,8                                                             
         LA    R3,OTHERS                                                        
         LA    R2,P2+35                                                         
         BAS   RE,EDITIT                                                        
         BAS   RE,PRINTIT                                                       
         B     EXIT                                                             
         EJECT                                                                  
EDITIT   NTR1                                                                   
*                                                                               
EDT15    SR    R0,R0                                                            
         L     R1,0(R3)                                                         
         D     R0,=F'10'                                                        
         ST    R1,0(R3)                                                         
         EDIT  (B4,0(R3)),(6,0(R2)),ALIGN=LEFT                                  
         LA    R2,10(R2)                                                        
         LA    R3,4(R3)                                                         
         BCT   R4,EDT15                                                         
         B     EXIT                                                             
*                                                                               
ADDIT    NTR1                                                                   
         L     R1,0(R4)                                                         
         A     R1,0(R2)                                                         
         ST    R1,0(R2)                                                         
         B     EXIT                                                             
*                                                                               
PRINTIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
HDRTN    NTR1                                                                   
         LA    R2,H7+20                                                         
         MVC   1(4,R2),=C'CODE'                                                 
         OC    DTCDSV+4(2),DTCDSV+4                                             
         BZ    *+10                                                             
         MVC   1(4,R2),=C'DATE'                                                 
         MVC   5(5,R2),=C'/TYPE'                                                
         MVC   25(5,R2),=C'TOTAL'                                               
         MVC   36(5,R2),=C'18-34'                                               
         MVC   46(5,R2),=C'18-49'                                               
         MVC   56(5,R2),=C'25-54'                                               
         MVC   66(5,R2),=C'35-64'                                               
         MVC   76(3,R2),=C'55+'                                                 
*                                                                               
         CLI   BOXSET,C'Y'                                                      
         BE    HDX                                                              
         MVI   BOXSET,C'Y'                                                      
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LTR   R1,R1               IS ABOX ZEROS                                
         BZ    HDX                 YES/ ON-LINE SKIP BOXES                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         LA    R5,BOXCOLS                                                       
         LA    R5,20(R5)                                                        
         MVI   0(R5),C'L'                                                       
         LA    R5,BOXCOLS                                                       
         LA    R5,111(R5)                                                       
         MVI   0(R5),C'R'                                                       
         SPACE                                                                  
         LA    R5,BOXROWS                                                       
         LA    R5,5(R5)                                                         
         MVI   0(R5),C'T'                                                       
         LA    R5,2(R5)                                                         
         MVI   0(R5),C'M'                                                       
         LA    R5,46(R5)                                                        
         MVI   0(R5),C'B'                                                       
         SPACE                                                                  
HDX      B     EXIT                 (XIT1)                                      
         EJECT                                                                  
***********************************                                             
* RESET BOXES TO GIVE MIDLINES                                                  
*                                                                               
BOXRESET NTR1                                                                   
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LTR   R1,R1               IS ABOX ZEROS                                
         BZ    BXIT                YES/ ON-LINE SKIP BOXES                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         LA    R5,BOXCOLS                                                       
         LA    R5,20(R5)                                                        
         MVI   0(R5),C'L'                                                       
         LA    R5,BOXCOLS                                                       
         LA    R5,111(R5)                                                       
         MVI   0(R5),C'R'                                                       
         SPACE                                                                  
         LA    R5,BOXROWS                                                       
         LA    R5,5(R5)                                                         
         MVI   0(R5),C'T'                                                       
         LA    R5,2(R5)                                                         
         MVI   0(R5),C'M'                                                       
         LA    R5,46(R5)                                                        
         MVI   0(R5),C'B'                                                       
         LA    R5,BOXROWS                                                       
         ZIC   R1,LINE                                                          
         C     R1,=F'45'         SKIP EXTRA LINE IF AT END OF PAGE              
         BNL   BXIT                                                             
         CLI   SPLCOM,C'Y'                                                      
         BNE   *+8                                                              
         LA    R1,1(R1)                                                         
         AR    R5,R1                                                            
         MVI   0(R5),C'M'                                                       
BXIT     B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,60,C'UNIVERSE LIST'                                           
         SSPEC H2,60,C'-------------'                                           
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
WORKD    DSECT                     MYWORK AREA  ANETWS2+500                     
DTCDSV   DS    CL6                                                              
TYPESV   DS    CL1                                                              
BOXSET   DS    CL1                                                              
CDS      DS    CL1                                                              
         DS    0F                                                               
RELO     DS    F                                                                
*                                                                               
TABLE    DS    0F                  TABLE OF UNIVERSES                           
*                                                                               
TBLWMN   DS    F                   WMTOT                                        
         DS    F                   WM1834                                       
         DS    F                   WM1849                                       
         DS    F                   WM2554                                       
         DS    F                   WM3564                                       
         DS    F                   WM55+                                        
*                                                                               
TBLMEN   DS    F                   MENTOT                                       
         DS    F                   MEN1834                                      
         DS    F                   1849                                         
         DS    F                   2554                                         
         DS    F                   3564                                         
         DS    F                   55+                                          
*                                                                               
ALLTOT   DS    F                   ALLTOT                                       
         DS    F                   ALL1834                                      
         DS    F                   ALL1849                                      
         DS    F                   2554                                         
         DS    F                   3564                                         
         DS    F                   55+                                          
*                                                                               
OTHERS   DS    F                   HOMES                                        
         DS    F                   ALL+2                                        
         DS    F                   LOH                                          
         DS    F                   WWORK                                        
         DS    F                   TEENS                                        
         DS    F                   FM12-17                                      
         DS    F                   CHILD                                        
         DS    F                   CH6-11                                       
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDD6D                                                       
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
       ++INCLUDE SPGENUNIV                                                      
       ++INCLUDE NEGENUSER                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046NEMED86S  05/01/02'                                      
         END                                                                    
