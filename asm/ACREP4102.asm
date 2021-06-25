*          DATA SET ACREP4102  AT LEVEL 020 AS OF 05/01/02                      
*PHASE AC4102A,+0                                                               
*INCLUDE ACCEDIT                                                                
         TITLE 'PROFILE EXCEPTION REPORT'                                       
AC4102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC4102                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    R9,SPACEND                                                       
         SR    R3,R3                                                            
         EJECT                                                                  
*              MAIN CONTROL                                                     
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BNE   PE2                                                              
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         SPACE 3                                                                
PE2      CLI   MODE,PROCLEVA       CLIENT?                                      
         BNE   PE10                                                             
         MVI   SPACING,2           SPACE BEFORE CLIENT                          
         GOTO1 ACREPORT                                                         
         L     R8,ADLVASUP         PROFILE ELEMENT                              
         LTR   R8,R8                                                            
         BZ    XIT                 SHOULDN'T HAPPEN                             
         BAS   RE,FORMAT           FORMAT PROFILE ELEMENT FOR PRINTING          
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              HANDLE A PRODUCT                                                 
         SPACE 2                                                                
PE10     CLI   MODE,PROCLEVB                                                    
         BNE   PE20                                                             
         L     R8,ADLVBSUP                                                      
         LTR   R8,R8               ANY PROFILE ELEMENT?                         
         BNZ   PE12                                                             
         CLI   QOPT1,C'P'          PRINT EVERY PRODUCT/JOB                      
         BNE   XIT                                                              
         BAS   RE,FORMAT           PRINT JUST THE NUMBER/NAME                   
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         SPACE 3                                                                
PE12     L     R7,ADLVASUP         POINT TO CLIENT PROFILE                      
         BAS   RE,COMPARE          TRY TO MATCH FIELDS                          
         BAS   RE,FORMAT                                                        
         OC    P+37(58),SPACES     CHANGE BINARY ZEROS TO SPACES                
         CLI   QOPT1,C'X'          ONLY PRINT OVERRIDES                         
         BNE   PE14                                                             
         CLC   P+37(58),SPACES     ANY OVERRIDE DATA TO PRINT                   
         BE    XIT                 NO - XIT                                     
PE14     DS    0H                                                               
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              HANDLE A JOB                                                     
         SPACE 2                                                                
PE20     CLI   MODE,PROCACC                                                     
         BNE   XIT                                                              
         L     R8,ADLVCSUP                                                      
         LTR   R8,R8               ANY JOB PROFILE                              
         BNZ   PE22                                                             
         CLI   QOPT1,C'P'          PRINT THE LOT?                               
         BNE   XIT                                                              
         BAS   RE,FORMAT                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         SPACE 3                                                                
PE22     L     R7,ADLVBSUP                                                      
         LTR   R7,R7               ANY PRODUCT PROFILE                          
         BZ    PE24                                                             
         BAS   RE,COMPARE                                                       
         SPACE 2                                                                
PE24     L     R7,ADLVASUP         CLIENT PROFILE                               
         BAS   RE,COMPARE          COMPARE AGAIN                                
         BAS   RE,FORMAT                                                        
         OC    P+37(58),SPACES     CHANGE BINARY ZEROS TO SPACES                
         CLI   QOPT1,C'X'          ONLY PRINT OVERRIDES                         
         BNE   PE26                                                             
         CLC   P+37(58),SPACES     ANY OVERRIDE DATA TO PRINT                   
         BNE   PE26                YES                                          
         MVC   P,SPACES                                                         
         B     XIT                                                              
PE26     DS    0H                                                               
         GOTO1 ACREPORT                                                         
         CLC   QUESTOR(10),=C'CLEAR THEM'                                       
         BNE   XIT                                                              
         MVI   MODE,WRITACC                                                     
         SPACE 3                                                                
XIT      XMOD1 1                                                                
         EJECT                                                                  
*              FORMAT A PROFILE ELEMENT FOR PRINTING                            
         SPACE 2                                                                
FORMAT   NTR1                                                                   
         L     R7,ADLVANAM                                                      
         L     R4,ADHEIRA                                                       
         CLI   MODE,PROCLEVA                                                    
         BE    FORM2                                                            
         SPACE 1                                                                
         L     R7,ADLVBNAM                                                      
         L     R4,ADHEIRB                                                       
         CLI   MODE,PROCLEVB                                                    
         BE    FORM2                                                            
         SPACE 1                                                                
         L     R7,ADLVCNAM                                                      
         L     R4,ADACC                                                         
         SPACE 1                                                                
FORM2    L     R5,ADLDGHIR                                                      
         USING ACNAMED,R7                                                       
         IC    R3,ACNMLEN                                                       
         SH    R3,=H'2'                                                         
         GOTO1 CHOPPER,DMCB,((R3),ACNMNAME),(20,P+16),(C'P',2)                  
         SPACE 2                                                                
         GOTO1 =V(ACCEDIT),DMCB,(R4),(R5),P+1,RR=RB                             
         LTR   R8,R8               ONLY PRINT NAME/NUMBER                       
         BZ    FORMEND                                                          
         USING ACPROFD,R8                                                       
         MVC   P+37(12),ACPRRECV+3                                              
         MVC   P+50(12),ACPRCOST+3                                              
         SPACE 1                                                                
         LA    R7,TABLE            FIND BILLING TYPE                            
FORM4    CLI   0(R7),X'FF'                                                      
         BE    FORM7                                                            
         CLC   ACPRBILL,0(R7)                                                   
         BE    FORM6                                                            
         LA    R7,L'TABLE(R7)                                                   
         B     FORM4                                                            
FORM6    MVC   P+63(11),1(R7)                                                   
         SPACE 1                                                                
         CLI   ACPRBILL,C'S'       DEAL WITH CASH FIELDS IN BILL TYPES          
         BE    FORM6A                                                           
         CLI   ACPRBILL,C'F'                                                    
         BNE   FORM6B                                                           
FORM6A   MVI   P+64,C','                                                        
         MVC   P+65(6),SPACES                                                   
         EDIT  (B4,ACPRBLAM),(9,DMCB),ALIGN=LEFT                                
         LR    R3,R0                                                            
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+65(0),DMCB                                                     
         B     FORM7                                                            
         SPACE 2                                                                
FORM6B   CLI   ACPRBILL,C'E'                                                    
         BNE   FORM7                                                            
         MVI   P+64,C','                                                        
         MVC   P+65(6),SPACES                                                   
         EDIT  (B4,ACPRBLAM),(5,P+65),2                                         
         SPACE 1                                                                
FORM7    MVC   P+78(2),ACPROFFC                                                 
         SPACE 1                                                                
         LA    R6,P+84                                                          
         LA    R7,ACPRUNBL                                                      
         CLC   0(2,R7),=X'0000'                                                 
         BE    FORMEND                                                          
         CLC   0(2,R7),=C'  '                                                   
         BE    FORMEND                                                          
         LA    R5,5                                                             
         MVC   0(2,R6),0(R7)                                                    
         LA    R6,2(R6)                                                         
FORM8    LA    R7,2(R7)                                                         
         CLC   0(2,R7),=X'0000'                                                 
         BE    FORMEND                                                          
         CLC   0(2,R7),=C'  '                                                   
         BE    FORMEND                                                          
         MVI   0(R6),C','                                                       
         MVC   1(2,R6),0(R7)                                                    
         LA    R6,3(R6)                                                         
         BCT   R5,FORM8                                                         
FORMEND  XIT1                                                                   
         SPACE 3                                                                
TABLE    DS    0CL12               TABLE OF BILLING TYPES                       
         DC    C'TTOTAL      '                                                  
         DC    C'SSPECIAL    '                                                  
         DC    C'EESTIMATE   '                                                  
         DC    C'UUNBILLABLE '                                                  
         DC    C'PPROGRESSIVE'                                                  
         DC    C'FFEE        '                                                  
         DC    C'IONE-LINE   '                                                  
         DC    C'CCLIENT     '                                                  
         DC    X'FF'                                                            
         EJECT                                                                  
*              COMPARES PROFILES & ZEROISE ANY EQUAL FIELDS                     
         SPACE 2                                                                
*              R8 POINTS TO LOW LEVEL PROFILE                                   
*              R7 POINTS TO HIGHER LEVEL                                        
         SPACE 1                                                                
COMPARE  NTR1                                                                   
         CLC   ACPRGRUP,2(R7)                                                   
         BNE   *+10                                                             
         XC    ACPRGRUP,ACPRGRUP                                                
         SPACE 1                                                                
         CLC   ACPRRECV,5(R7)                                                   
         BNE   *+10                                                             
         XC    ACPRRECV,ACPRRECV                                                
         SPACE 1                                                                
         CLC   ACPRCOST,20(R7)                                                  
         BNE   *+10                                                             
         XC    ACPRCOST,ACPRCOST                                                
         SPACE 1                                                                
         CLC   ACPRBILL,37(R7)                                                  
         BNE   *+10                                                             
         XC    ACPRBILL,ACPRBILL                                                
         SPACE 1                                                                
         CLC   ACPRBLAM,38(R7)                                                  
         BNE   *+10                                                             
         XC    ACPRBLAM,ACPRBLAM                                                
         SPACE 1                                                                
         CLC   ACPROFFC,ACPROFFC-ACPROFD(R7)                                    
         BNE   *+10                                                             
         XC    ACPROFFC,ACPROFFC                                                
         SPACE 1                                                                
         CLC   ACPROFFC,SPACES                                                  
         BH    *+10                                                             
         XC    ACPROFFC,ACPROFFC                                                
         SPACE 1                                                                
         CLC   ACPRUNBL,43(R7)                                                  
         BNE   *+10                                                             
         XC    ACPRUNBL,ACPRUNBL                                                
         SPACE 2                                                                
         CLC   ACPRUNBL,SPACES                                                  
         BNE   *+10                                                             
         XC    ACPRUNBL,ACPRUNBL                                                
         SPACE 2                                                                
         SPACE 1                                                                
         CLC   ACPRBLPR,55(R7)                                                  
         BNE   *+10                                                             
         XC    ACPRBLPR,ACPRBLPR                                                
         XIT1                                                                   
         EJECT                                                                  
*        ACREPWORKD                                                             
*        ACGENBOTH                                                              
*        ACGENMODES                                                             
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020ACREP4102 05/01/02'                                      
         END                                                                    
