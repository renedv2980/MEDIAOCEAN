*          DATA SET CTREP3002  AT LEVEL 014 AS OF 05/01/02                      
*PHASE CT3002A                                                                  
         TITLE 'CPP XTRACT RULES'                                               
CT3002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CPPX**                                                       
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
         MVI   FCRDXT,C'Y'                                                      
         CLI   MODE,REQFRST                                                     
         BNE   CP1                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         B     XIT                                                              
         SPACE 2                                                                
CP1      CLI   MODE,PROCXT                                                      
         BNE   XIT                                                              
         EJECT                                                                  
*              PROCESS A CPP EXTRACT RECORD                                     
         SPACE 3                                                                
         L     R2,ADRECORD                                                      
         USING CTXREC,R2                                                        
         CLC   CTXKAGY,LASTKAGY    EDIT THE KEY FIELDS                          
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   HEAD4+8(2),CTXKAGY                                               
         MVC   LASTKAGY,CTXKAGY                                                 
         MVC   P,SPACES                                                         
         MVC   P+2(3),CTXKCLT                                                   
         MVC   P+11(3),CTXKPRD                                                  
         EDIT  (1,CTXKEST),(3,P+20),ZERO=BLANK                                  
         OC    P(25),SPACES                                                     
         OC    CTXKSTRT,CTXKSTRT                                                
         BZ    CP2                                                              
         GOTO1 DATCON,DMCB,(1,CTXKSTRT),(8,P+28)                                
         SPACE 2                                                                
CP2      OC    CTXKEND,CTXKEND                                                  
         BZ    CP4                                                              
         GOTO1 DATCON,DMCB,(1,CTXKEND),(8,P+37)                                 
         SPACE 2                                                                
CP4      LR    R4,R2               NOW CHECK ELEMENTS                           
         AH    R4,DATADISP                                                      
         SR    R3,R3                                                            
         LA    R5,5                                                             
         LA    R6,P+71                                                          
         SPACE 2                                                                
CP6      CLI   0(R4),0                                                          
         BNE   CP8                                                              
         GOTO1 REPORT                                                           
         MVC   HEAD4+8(2),CTXKAGY                                               
         BASR  RE,RF                                                            
         B     XIT                                                              
         SPACE 2                                                                
CP8      CLI   0(R4),X'74'         DEMO LIST                                    
         BNE   CP20                                                             
         USING CTDXD,R4                                                         
         ZIC   R7,CTDXLEN                                                       
         SH    R7,=H'2'                                                         
         LA    R8,CTDXLIST                                                      
         LA    R9,AREA                                                          
         MVI   AREA,C' '                                                        
         MVC   AREA+1(249),AREA                                                 
         SPACE 2                                                                
CP10     TM    0(R8),X'F0'                                                      
         BO    CP12                                                             
         MVC   0(4,R9),=C'NONE'                                                 
         LA    R9,5(R9)                                                         
         CLI   0(R8),0                                                          
         BE    CP14                                                             
         SH    R9,=H'5'                                                         
         MVC   0(4,R9),SPACES                                                   
         EDIT  (1,(R8)),(3,(R9)),ALIGN=LEFT                                     
         AR    R9,R0                                                            
         LA    R9,1(R9)                                                         
         B     CP14                                                             
         SPACE 2                                                                
CP12     MVC   0(5,R9),=C'FIRST'                                                
         LA    R9,6(R9)                                                         
         CLI   0(R8),X'F1'                                                      
         BE    CP14                                                             
         SH    R9,=H'6'                                                         
         MVC   0(6,R9),=C'SECOND'                                               
         LA    R9,7(R9)                                                         
         CLI   0(R8),X'F2'                                                      
         BE    CP14                                                             
         SH    R9,=H'7'                                                         
         MVC   0(5,R9),=C'THIRD'                                                
         MVI   5(R9),C' '                                                       
         LA    R9,6(R9)                                                         
         CLI   0(R8),X'F3'                                                      
         BE    CP14                                                             
         SH    R9,=H'6'                                                         
         MVC   0(6,R9),=C'FOURTH'                                               
         LA    R9,7(R9)                                                         
         SPACE 2                                                                
CP14     LA    R8,1(R8)                                                         
         BCT   R7,CP10                                                          
         GOTO1 CHOPPER,DMCB,(250,AREA),(23,P+47),(C'P',4)                       
         B     CP30                                                             
         SPACE 2                                                                
CP20     CLI   0(R4),X'76'         PROGTYP                                      
         BNE   CP30                                                             
         USING CTPQD,R4                                                         
         MVC   0(3,R6),CTPQDP                                                   
         OI    0(R6),X'40'                                                      
         MVI   3(R6),C'='                                                       
         MVC   4(1,R6),CTPQCODE                                                 
         LA    R6,6(R6)                                                         
         BCT   R5,CP30                                                          
         GOTO1 REPORT                                                           
         LA    R6,P+71                                                          
         LA    R5,5                                                             
         SPACE 2                                                                
CP30     IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     CP6                                                              
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
LASTKAGY DS    CL2                                                              
AREA     DS    CL250                                                            
         PRINT OFF                                                              
       ++INCLUDE CTREPMODES                                                     
       ++INCLUDE CTREPWORKD                                                     
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014CTREP3002 05/01/02'                                      
         END                                                                    
