*          DATA SET NEPUP03    AT LEVEL 010 AS OF 05/01/02                      
*          DATA SET NEPUP03    AT LEVEL 023 AS OF 05/15/90                      
*PHASE T32203A,*                                                                
         TITLE 'T32203 - PLAN REPORT'                                           
T32203   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32203**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,VKEY                                                          
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY FOR REPORT                                          
         SPACE 3                                                                
VKEY     NTR1                                                                   
         LA    R2,PUPCLIH          CLIENT (REQUIRED)                            
         GOTO1 VVALCLT                                                          
         MVC   PUPCLIN,CLTNAME                                                  
         OI    PUPCLINH+6,X'80'                                                 
         SPACE 1                                                                
*                                  REST OF FIELDS OPTIONAL                      
         XC    NETWORK,NETWORK                                                  
         MVI   DPTCODE,0                                                        
         XC    PLANCODE,PLANCODE                                                
         LA    R2,PUPNETH          NETWORK                                      
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 VVALNET                                                          
         SPACE 1                                                                
         LA    R2,PUPDPTH          DAYPART                                      
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 VVALDPT                                                          
         MVC   PUPDPT,DPTNAME                                                   
         OI    PUPDPTH+6,X'80'                                                  
         SPACE 1                                                                
         LA    R2,PUPPLANH         PLAN                                         
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 ANY                                                              
         MVC   PLANCODE,WORK                                                    
         B     XIT                                                              
         EJECT                                                                  
*              HANDLE I/O FOR REPORT                                            
         SPACE 3                                                                
PREP     NTR1                                                                   
         LA    R4,KEY                                                           
         USING NPLKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVI   NPLKTYPE,X'20'      FILL PLAN KEY                                
         MVC   NPLKAM,BINAGYMD                                                  
         MVC   NPLKCLT,CLTCOMP                                                  
         MVC   NPLKNET,NETWORK                                                  
         MVC   NPLKDPT,DPTCODE                                                  
         MVC   NPLKPLAN,WORK                                                    
         LA    R1,HEDSPECS         INITIALIZE                                   
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         GOTO1 HIGH                                                             
         B     PREP4                                                            
         SPACE 1                                                                
PREP2    GOTO1 SEQ                                                              
         SPACE 1                                                                
PREP4    CLC   KEY(4),KEYSAVE      MUST HAVE MATCH ON CLIENT                    
         BNE   PREPEND                                                          
         CLI   PUPNETH+5,0         OPTIONAL NETWORK                             
         BE    PREP6                                                            
         CLC   KEY(9),KEYSAVE                                                   
         BNE   PREPEND                                                          
         CLI   PUPDPTH+5,0         OPTIONAL DAYPART                             
         BE    PREP6                                                            
         CLC   KEY(11),KEYSAVE                                                  
         BNE   PREPEND                                                          
         SPACE                                                                  
PREP6    GOTO1 GETREC                                                           
         GOTO1 VEXTPLAN                                                         
         BAS   RE,FORM                                                          
         B     PREP2                                                            
         SPACE 1                                                                
PREPEND  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT AND PRINT A PLAN                               
         SPACE 3                                                                
FORM     NTR1                                                                   
         L     R4,AIO                                                           
         USING NPLRECD,R4                                                       
         LA    R2,P+3              DISPLAY NETWORK                              
         MVC   0(4,R2),NPLKNET                                                  
         SPACE 1                                                                
         LA    R2,6(R2)            DAYPART                                      
         MVC   WORK,NPLKDPT                                                     
         GOTO1 VLUPDPT                                                          
         MVC   0(7,R2),DPTNAME                                                  
         SPACE 1                                                                
         LA    R2,8(R2)            PLAN CODE                                    
         MVC   0(4,R2),NPLKPLAN                                                 
         SPACE 1                                                                
         LA    R2,5(R2)            PLAN NAME                                    
*        MVC   0(16,R2),PLANNAME                                                
         MVC   0(11,R2),PLANNAME                                                
         MVC   13(3,R2),NPLNFILT   PLAN FILTER                                  
         SPACE 1                                                                
         LA    R2,17(R2)           PLAN YEAR                                    
*        MVC   0(2,R2),=C'19'                                                   
*        EDIT  (1,PLANYEAR),(2,2(R2))                                           
         ZIC   R1,PLANYEAR                                                      
         AH    R1,=H'1900'          Y2K FIX                                     
         EDIT  (R1),(4,0(R2))                                                   
         SPACE 1                                                                
         LA    R2,5(R2)            UNIVERSE CODE                                
         OC    PLANUNIV,PLANUNIV                                                
         BZ    FORM2                                                            
         MVC   DUB+6(2),PLANUNIV                                                
         L     R1,DUB+4                                                         
         SLL   R1,4                                                             
         ST    R1,DUB+4                                                         
         OI    DUB+7,X'0F'                                                      
         CVB   R1,DUB                                                           
         EDIT  (R1),(4,0(R2)),ALIGN=LEFT                                        
         SPACE 1                                                                
FORM2    LA    R2,5(R2)            SECONDS LENGTHS                              
         BAS   RE,FLENS                                                         
         LA    R2,13(R2)           HUT YEAR                                     
         EDIT  (1,PLANHTYR),(2,0(R2))                                           
         CLI   PLANHTNO,2                                                       
         BL    FORM4                                                            
         MVI   2(R2),C','                                                       
         EDIT  (1,PLANHTNO),(1,3(R2))                                           
         SPACE 1                                                                
FORM4    MVC   5(1,R2),PLANHTSC    HUT SCHEME                                   
         MVC   8(1,R2),PLANHTAV    HUT AVERAGE                                  
         SPACE 1                                                                
         LA    R2,11(R2)           DEMOS                                        
         BAS   RE,FDEMS                                                         
         LA    R2,16(R2)           GUARANTEED CPM                               
         OC    GUARCPM,GUARCPM                                                  
         BZ    FORM6                                                            
         EDIT  (4,GUARCPM),(6,0(R2)),2,FLOAT=$                                  
         SPACE 1                                                                
FORM6    LA    R2,7(R2)                                                         
         BAS   RE,FBUDS            BUDGETS                                      
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT LENGTHS                                        
         SPACE 3                                                                
FLENS    NTR1                                                                   
         LR    R5,R2                                                            
         LA    R3,PLANLENS                                                      
         ZIC   R4,PLANNLEN                                                      
         SPACE 1                                                                
FLENS2   EDIT  (1,0(R3)),(3,0(R5))                                              
         LA    R3,1(R3)                                                         
         LA    R5,4(R5)                                                         
         BCT   R4,FLENS2                                                        
         SPACE 1                                                                
         GOTO1 SQUASHER,DMCB,(R2),15                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT DEMOS                                          
         SPACE 3                                                                
FDEMS    NTR1                                                                   
         GOTO1 VSETDB                                                           
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'T'                                                    
         LA    R3,DEMOS                                                         
         ZIC   R4,NDEMOS                                                        
         SR    R5,R5                                                            
         SPACE 1                                                                
FDEMS2   GOTO1 DEMOCON,DMCB,(0,0(R3)),(10,WORK),(C'S',DBLOCK)                   
*        CLI   WORK+2,C'.'    IF NAD DEMO                                       
         BNE   FDEMS2B                                                          
         CLI   0(R3),0        IF NAD DEMO                                       
         BE    FDEMS2B                                                          
         CH    R5,=H'0'       ONLY ONE PER LINE                                 
         BNE   FDEMS2A                                                          
         MVC   0(10,R2),WORK                                                    
         LA    R2,132(R2)                                                       
         B     FDEMS4                                                           
FDEMS2A  LA    R2,132-8(R2)                                                     
         MVC   0(10,R2),WORK                                                    
         LA    R2,132(R2)                                                       
         SR    R5,R5                                                            
         B     FDEMS4                                                           
FDEMS2B  MVC   0(7,R2),WORK                                                     
         LA    R2,8(R2)                                                         
         LA    R5,1(R5)                                                         
         CH    R5,=H'2'                                                         
         BNE   FDEMS4                                                           
         LA    R2,132-16(R2)                                                    
         SR    R5,R5                                                            
         SPACE 1                                                                
FDEMS4   LA    R3,3(R3)                                                         
         BCT   R4,FDEMS2                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT BUDGETS                                        
         SPACE 3                                                                
FBUDS    NTR1                                                                   
         MVI   LENGTH,0                                                         
         GOTO1 VEXTBUDG                                                         
         LA    R3,BUDGETS                                                       
         LA    R4,4                                                             
         SPACE 1                                                                
FBUDS2   BAS   RE,FBUDS4                                                        
         LA    R2,9(R2)                                                         
         LA    R3,20(R3)                                                        
         BCT   R4,FBUDS2                                                        
         B     XIT                                                              
         SPACE 1                                                                
FBUDS4   NTR1                                                                   
*                                  FIRST SHOW TOTAL BUDGET                      
         EDIT  (4,0(R3)),(8,0(R2))                                              
         OC    4(16,R3),4(R3)                                                   
         BZ    XIT                                                              
         SPACE 1                                                                
*                                  BUDGETS ARE AVAILABLE BY LENGTH              
         LA    R3,4(R3)            SO DISPLAY THE ANALYZED BUDGETS              
         LA    R4,4                                                             
         SPACE 1                                                                
FBUDS6   EDIT  (4,0(R3)),(8,0(R2))                                              
         LA    R2,132(R2)                                                       
         LA    R3,4(R3)                                                         
         BCT   R4,FBUDS6                                                        
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK                                                    
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVC   HEAD4+9(3),PUPCLI                                                
         MVC   HEAD4+13(20),PUPCLIN                                             
         SPACE 1                                                                
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         SPACE 1                                                                
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         SPACE 1                                                                
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+08,C'C'                                                  
         MVI   BOXCOLS+16,C'C'                                                  
         MVI   BOXCOLS+21,C'C'                                                  
         MVI   BOXCOLS+33,C'C'                                                  
         MVI   BOXCOLS+38,C'C'                                                  
         MVI   BOXCOLS+43,C'C'                                                  
         MVI   BOXCOLS+48,C'C'                                                  
         MVI   BOXCOLS+61,C'C'                                                  
         MVI   BOXCOLS+72,C'C'                                                  
         MVI   BOXCOLS+88,C'C'                                                  
         MVI   BOXCOLS+95,C'C'                                                  
         MVI   BOXCOLS+131,C'R'                                                 
         B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 3                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
MYCURSOR MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         GOTO1 VCURSERR            AND POSITIONING CURSOR                       
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRXIT                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              SPECS FOR PHASE                                                  
         SPACE 3                                                                
HEDSPECS DS    0D                                                               
         SSPEC H1,2,C'NETWORK UPFRONT SYSTEM'                                   
         SSPEC H2,2,C'----------------------'                                   
         SSPEC H1,52,C'PLAN REPORT'                                             
         SSPEC H2,52,C'-----------'                                             
         SSPEC H4,2,C'CLIENT'                                                   
         SPACE 1                                                                
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,99,RUN                                                        
         SSPEC H5,99,REPORT                                                     
         SSPEC H5,115,PAGE                                                      
         SPACE 1                                                                
***      SSPEC H8,2,C'NETWORK DAYPART PLAN    PLAN NAME     PLAN UNIV'          
         SSPEC H8,2,C'NETWORK DAYPART PLAN   NAME      FILT PLAN UNIV'          
         SSPEC H9,2,C'                                      YEAR CODE'          
         SSPEC H8,52,C'SECONDS    ---HUT----  DEMOGRAPHICS    GUAR'             
         SSPEC H9,52,C'LENGTHS    YRS SC AVE                  CPM.'             
         SSPEC H8,99,C'--------QUARTERLY BUDGETS--------'                       
         SSPEC H9,99,C'FOURTH    FIRST   SECOND    THIRD'                       
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEPUPALLN                                                      
         EJECT                                                                  
       ++INCLUDE NEPUPF3D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010NEPUP03   05/01/02'                                      
         END                                                                    
