*          DATA SET CTREP7202  AT LEVEL 015 AS OF 05/01/02                      
*PHASE CT7202A                                                                  
         TITLE 'CT7202 - EFFECTIVE REPORT PROFILES'                             
CT7202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CT7202                                                       
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
         CLI   MODE,REQFRST                                                     
         BNE   EP2                                                              
         MVC   PAGE,=H'1'                                                       
         MVI   FSW,C'Y'                                                         
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL RECORDS                                                  
         SPACE 3                                                                
EP2      CLI   MODE,PROCUSER                                                    
         BNE   EP10                                                             
         L     R2,ADRECORD                                                      
         USING CTUREC,R2                                                        
         OC    CTUKAGY,CTUKAGY     IS THIS A FIELD DESCRIPTION REC.             
         BNZ   EP4                                                              
         CLI   FSW,C'Y'            IF NOT FIRST TIME,                           
         BE    EP3                                                              
         CLI   ANYUSER,C'Y'                                                     
         BNE   EP3                                                              
         BAS   RE,SPLAT            PRINT PROFILES                               
         SPACE 2                                                                
EP3      MVI   ANYUSER,C'N'                                                     
         BAS   RE,CLEAR                                                         
         MVI   FSW,C'N'                                                         
         L     R3,=A(SAVEFD)       SAVE RECORD FOR LATER                        
         AR    R3,RB                                                            
         MOVE  ((R3),1000),(R2)                                                 
         B     XIT                                                              
         SPACE 2                                                                
EP4      L     R3,=A(PROFPOOL)     THIS IS A USER RECORD                        
         AR    R3,RB                                                            
         L     R5,POOLMAX                                                       
*                                  OPTION TO FILTER ON AGENCY/MEDIA             
         CLC   QAGENCY,SPACES                                                   
         BE    EP6                                                              
         CLC   CTUKAGY,QAGENCY                                                  
         BNE   XIT                                                              
         CLC   QMEDIA,SPACES                                                    
         BE    EP6                                                              
         CLC   CTUKMED,QMEDIA                                                   
         BNE   XIT                                                              
         SPACE 2                                                                
EP6      OC    0(24,R3),0(R3)      FIND A SLOT IN POOL                          
         BZ    EP8                                                              
         LA    R3,24(R3)                                                        
         BCT   R5,EP6                                                           
         DC    H'0'                                                             
         SPACE 2                                                                
EP8      MVC   0(7,R3),CTUKAGY     POP IT IN                                    
         LR    R4,R2                                                            
         MVI   ELCODE,X'72'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTPVD,R4                                                         
         MVC   6(18,R3),CTPVSTAT                                                
         MVI   ANYUSER,C'Y'                                                     
         B     XIT                                                              
EP10     CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         BAS   RE,SPLAT                                                         
         BAS   RE,CLEAR                                                         
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
ELCODE   DC    X'00'                                                            
         DC    X'00'                                                            
         EJECT                                                                  
*              ROUTINE TO CLEAR PROFILE POOL & CONTROL PRINT                    
         SPACE 3                                                                
CLEAR    L     R3,=A(PROFPOOL)                                                  
         AR    R3,RB                                                            
         L     R5,POOLMAX                                                       
         SPACE 2                                                                
CLEAR2   XC    0(24,R3),0(R3)                                                   
         LA    R3,24(R3)                                                        
         BCT   R5,CLEAR2                                                        
         BR    RE                                                               
         SPACE 2                                                                
PRINTEM  NTR1                                                                   
         L     R2,=A(SAVEFD)                                                    
         AR    R2,RB                                                            
         MVC   HEAD4+17(4),CTUKSYS                                              
         CLI   CTUKPAGE,0                                                       
         BE    PR2                                                              
         MVC   HEAD5+17(4),=C'PAGE'                                             
         EDIT  (1,CTUKPAGE),(3,HEAD5+22),ALIGN=LEFT                             
         SPACE 2                                                                
PR2      IC    R7,ELCODE                                                        
         MVI   ELCODE,X'01'                                                     
         LR    R4,R2                                                            
         BAS   RE,GETEL                                                         
         BNE   PR4                                                              
         USING CTACTD,R4                                                        
         MVC   HEAD5+84(9),=C'ACTIVE ON'                                        
         GOTO1 DATCON,DMCB,(3,CTACTDT),(8,HEAD5+94)                             
         SPACE 2                                                                
PR4      MVI   ELCODE,X'02'                                                     
         LR    R4,R2                                                            
         BAS   RE,GETEL                                                         
         BNE   PR6                                                              
         USING CTDSCD,R4                                                        
         SR    R3,R3                                                            
         IC    R3,CTDSCLEN                                                      
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   HEAD4+22(0),CTDSC                                                
         SPACE 2                                                                
PR6      STC   R7,ELCODE                                                        
         GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL REPORT FROM SAVED POOLS                       
         SPACE 2                                                                
SPLAT    NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         L     R2,=A(SAVEFD)                                                    
         AR    R2,RB                                                            
         MVC   SAVESYS,CTUKSYS                                                  
         MVI   ELCODE,X'70'                                                     
         LR    R4,R2                                                            
         USING CTFDD,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         SPACE 2                                                                
SP2      EDIT  (1,CTFDNUM),(2,P+3),FILL=0                                       
         SR    R3,R3                                                            
         IC    R3,CTFDLEN                                                       
         SH    R3,=H'27'                                                        
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+09(0),CTFDDESC                                                 
         MVC   P+41(1),CTFDTYPE                                                 
         MVC   P+46(20),CTFDLIST                                                
         LA    R6,P+69             VALUE DEFAULT                                
         LA    R3,CTFDDEF                                                       
         BAS   RE,VALUE                                                         
         MVI   P+72,C' '                                                        
         BAS   RE,PROF                                                          
         BAS   RE,NEXTEL                                                        
         BE    SP2                                                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL PROFILES                                      
         SPACE 3                                                                
PROF     NTR1                                                                   
         L     R2,=A(PROFPOOL)                                                  
         AR    R2,RB                                                            
         L     R5,POOLMAX                                                       
         SPACE 2                                                                
PROF2    OC    0(24,R2),0(R2)                                                   
         BNZ   PROF4                                                            
         CLC   P(60),SPACES                                                     
         BE    *+8                                                              
         BAS   RE,PRINTEM                                                       
         BAS   RE,PRINTEM                                                       
         B     XIT                                                              
         SPACE 2                                                                
PROF4    SR    R3,R3               FORMAT PROFILE DETAILS                       
         IC    R3,CTFDNUM                                                       
         MVC   P+78(2),0(R2)       AGENCY                                       
         CLI   SAVESYS,C'A'                                                     
         BNE   PROF6                                                            
         GOTO1 HEXOUT,DMCB,0(R2),P+78,1,=C'TOG'                                 
         MVC   P+87(2),1(R2)       UNIT/LEDGER                                  
         B     PROF7                                                            
PROF6    MVC   P+87(1),2(R2)                                                    
         OI    P+87,C' '                                                        
PROF7    OC    3(3,R2),3(R2)                                                    
         BZ    PROF8                                                            
         CLI   2(R2),0                                                          
         BNE   *+10                                                             
         MVC   P+87(3),=C'ALL'                                                  
         MVC   P+95(3),3(R2)                                                    
         TM    4(R2),X'C0'                                                      
         BNZ   PROF8                                                            
         EDIT  (B2,4(R2)),(4,P+96),FILL=0                                       
PROF8    DS    0H                                                               
         OC    P+77(25),SPACES                                                  
*        BAS   RE,ANYSTAT                                                       
         LA    R3,8-1(R3,R2)       FIELD NUM. INDEXES TO VALUE                  
         LA    R6,P+105                                                         
         BAS   RE,VALUE                                                         
         BAS   RE,PRINTEM                                                       
         LA    R2,24(R2)                                                        
         BCT   R5,PROF2                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EDIT A PROFILE VALUE                                  
         SPACE 3                                                                
VALUE    NTR1                                                                   
         TM    CTFDOTHR,X'40'                                                   
         BZ    *+12                                                             
         CLI   0(R3),0                                                          
         BE    VALX                                                             
         MVC   2(1,R6),0(R3)                                                    
         CLI   CTFDTYPE,C'C'                                                    
         BE    VALX                                                             
         GOTO1 HEXOUT,DMCB,0(R3),1(R6),1,=C'TOG'                                
         CLI   CTFDTYPE,C'X'                                                    
         BE    VALX                                                             
         EDIT  (B1,0(R3)),(3,0(R6))                                             
         OI    2(R6),X'F0'                                                      
VALX     TM    CTFDOTHR,X'80'                                                   
         BZ    *+8                                                              
         MVI   3(R6),C'*'          INDICATE DDS ONLY FIELD                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SEE IF THIS PROFILE IS NON-OVERRIDABLE                
         SPACE 2                                                                
ANYSTAT  NTR1                                                                   
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LH    R1,6(R2)            PROFILE BITS TO R1                           
         SLL   R1,16                                                            
         SRL   R1,16                                                            
         SR    R0,R0               FLOAT BIT IN R0                              
         LA    R0,1                                                             
         SLL   R0,16                                                            
         SPACE 2                                                                
ANYSTAT2 SRL   R0,1                R3 HAS FIELD NUM. (1-16)                     
         BCT   R3,ANYSTAT2                                                      
         NR    R0,R1               CRUNCH                                       
         LTR   R0,R0                                                            
         BZ    XIT                                                              
         MVI   P+103,C'*'                                                       
         B     XIT                                                              
         EJECT                                                                  
*              STORAGE, LTORG, CSECTS & DSECTS                                  
         SPACE 3                                                                
FSW      DC    C'Y'                                                             
ANYUSER  DC    C'N'                                                             
POOLMAX  DC    F'1000'                                                          
SAVESYS  DS    CL1                                                              
         LTORG                                                                  
         SPACE 2                                                                
SAVEFD   CSECT                                                                  
         DS    1000C                                                            
         SPACE 2                                                                
PROFPOOL CSECT                                                                  
         DS    24000C                                                           
         SPACE 2                                                                
         PRINT OFF                                                              
       ++INCLUDE CTREPWORKD                                                     
       ++INCLUDE CTREPMODES                                                     
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015CTREP7202 05/01/02'                                      
         END                                                                    
