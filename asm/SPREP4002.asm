*          DATA SET SPREP4002  AT LEVEL 029 AS OF 05/01/02                      
*PHASE SP4002A,*,NOAUTO                                                         
         TITLE 'ACTIVE MARKET REPORT - APPLICATION'                             
SP4002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SP4002                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         STM   R9,RB,SP40R9RB                                                   
         SPACE 1                                                                
* CONTROL SECTION *                                                             
         SPACE 1                                                                
         CLI   MODE,PROCBUY                                                     
         BE    SP300               FLAG MARKETS WITH BUYS AS ACTIVE             
         BH    CNTRL10                                                          
         SPACE 1                                                                
* 'FRST' BREAKS *                                                               
         SPACE 1                                                                
         CLI   MODE,CLTFRST                                                     
         BE    SP200               INITIALIZE FOR NEW CLIENT                    
         CLI   MODE,REQFRST                                                     
         BE    SP100               RELOCATE MARKET TABLE                        
         B     EXIT                                                             
         SPACE 1                                                                
* 'LAST' BREAKS *                                                               
         SPACE 1                                                                
CNTRL10  DS    0H                                                               
         CLI   MODE,PROCGOAL                                                    
         BE    SP400               FLAG MARKETS WITH GOALS AS ACTIVE            
         CLI   MODE,MGR1LAST                                                    
         BE    SP500               BUILD MARKET/MGRP TABLE                      
         CLI   MODE,CLTLAST                                                     
         BE    SP600               PRINT CLIENT'S ACTIVITY LISTING              
EXIT     XIT1                                                                   
         SPACE 1                                                                
PATCH1   DS    CL16                                                             
PATCH2   DS    CL16                                                             
         EJECT                                                                  
* SP100 - AT REQFRST, RELOCATE MKTTBL *                                         
         SPACE 1                                                                
SP100    DS    0H                                                               
         RELOC RELO                                                             
         L     RF,=V(MKTTBL)                                                    
         A     RF,RELO                                                          
         ST    RF,VMKTTBL                                                       
         L     R7,=V(BUFFALOC)                                                  
         A     R7,RELO                                                          
         ST    R7,VBUFFALO                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',(R7)                                        
         B     EXIT                                                             
         SPACE 2                                                                
* SP200 - AT CLTFRST, INITIALIZE MKTTBL AND MKTCTR *                            
         SPACE 1                                                                
SP200    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R0,120                                                           
         L     RF,VMKTTBL                                                       
         XC    0(250,RF),0(RF)                                                  
         LA    RF,250(RF)                                                       
         BCT   R0,*-10                                                          
         ST    RF,VMKTTBLX                                                      
         ZAP   MKTCTR,=P'0'                                                     
         MVC   PAGE,=H'1'                                                       
         B     EXIT                                                             
         SPACE 2                                                                
* SP300 - AT PROCBUY, FLAG ALL MARKETS WITH BUYS AS ACTIVE *                    
         SPACE 1                                                                
SP300    DS    0H                                                               
         XR    R2,R2                                                            
         ICM   R2,3,KEY+4          GET MARKET NUMBER                            
         A     R2,VMKTTBL                                                       
         OI    0(R2),X'80'         FLAG AS BOUGHT                               
         B     EXIT                                                             
         SPACE 2                                                                
* SP400 - AT PROCGOAL, FLAG ALL MARKETS WITH GOALS AS ACTIVE *                  
         SPACE 1                                                                
SP400    DS    0H                                                               
         XR    R2,R2                                                            
         ICM   R2,3,KEY+5          GET MARKET NUMBER                            
         A     R2,VMKTTBL                                                       
         OI    0(R2),X'40'         FLAG AS HAVING A GOAL                        
         B     EXIT                                                             
         EJECT                                                                  
* SP500 - AT MGR1LAST, BUILD A TABLE OF MARKETS *                               
*  AND THE MARKET GROUPS TO WHICH THEY BELONG   *                               
         SPACE 1                                                                
SP500    DS    0H                                                               
         L     R7,VBUFFALO                                                      
         LA    R8,KEY                                                           
         USING MKGKEY,R8                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(2),=XL2'0D82'                                                
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         MVC   KEY+3(2),SVCLT      CLT                                          
         GOTO1 HIGH                                                             
         B     SP520                                                            
         SPACE 1                                                                
SP510    DS    0H                                                               
         GOTO1 SEQ                                                              
         SPACE 1                                                                
SP520    DS    0H                                                               
         CLC   KEY(5),KEYSAVE      SAME OD82/A-M/CLT?                           
         BNE   SP530                                                            
         MVC   BUFMGID,MKGPMID     MARKET GROUP ID                              
         UNPK  DUB,MKGPMGRP                                                     
         MVC   BUFMGN,DUB+3        MARKGET GROUP NUMBER                         
         SR    RF,RF                                                            
         ICM   RF,3,MKGPMKT                                                     
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUFMKT,DUB                                                       
         GOTO1 BUFFALO,DMCB,=C'PUT',(R7),BUFREC                                 
         B     SP510                                                            
         SPACE 1                                                                
SP530    DS    0H                                                               
         B     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
* SP600 - AT CLTLAST, PRINT THAT CLIENT'S MARKET ACTIVITY REPORT *              
         SPACE 1                                                                
SP600    DS    0H                                                               
         L     R8,ADMARKET                                                      
         USING MKTREC,R8                                                        
         MVI   RCSUBPRG,1                                                       
         L     R2,VMKTTBL                                                       
         LA    R4,4                                                             
         B     SP630                                                            
         SPACE 1                                                                
SP610    DS    0H                                                               
         TM    0(R2),X'C0'         IS THIS AN ACTIVE MARKET?                    
         BZ    SP640                NO.                                         
         L     R0,VMKTTBL                                                       
         SR    R0,R2                                                            
         LPR   R0,R0               R0 HAS MARKET NUMBER                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(4,R3),DUB         PRINT MARKET NUMBER                          
         TM    0(R2),X'C0'         DOES THIS MKT HAVE BOTH BUYS & GOALS         
         BO    SP620                YES                                         
         MVI   4(R3),C'B'                                                       
         TM    0(R2),X'80'                                                      
         BO    SP620                NO. JUST BUYS                               
         MVI   4(R3),C'G'           NO. JUST GOALS                              
SP620    DS    0H                                                               
         MVI   5(R3),C'-'                                                       
         MVC   7(13,R3),=C'** UNKNOWN **'                                       
         BAS   RE,SPMKTNM          GET NAME OF MARKET                           
         CLC   KEY(15),MKTREC      IS THIS CORRECT MARKET RECORD                
         BNE   *+10                 NO.                                         
         MVC   7(24,R3),MKTNAME     YES.                                        
         LA    R3,33(R3)                                                        
         BCT   R4,SP640                                                         
         SPACE 1                                                                
         LR    R3,R6               POINT TO NEXT PRINT LINE                     
         LA    R4,4                                                             
         LA    R6,132(R6)                                                       
         MVI   0(R6),0                                                          
         LA    R6,132(R6)                                                       
         BCT   R7,SP640            HAVE WE BUILT 5 PRINT LINES?                 
         GOTO1 REPORT                                                           
SP630    DS    0H                                                               
         LA    R3,P1+1                                                          
         LA    R6,P3+1                                                          
         LA    R7,5                                                             
         MVI   P2,0                                                             
SP640    DS    0H                                                               
         LA    R2,1(R2)                                                         
         C     R2,VMKTTBLX         HAVE WE DONE ALL THE MARKETS?                
         BL    SP610                                                            
         SPACE 1                                                                
         CH    R4,=H'4'                                                         
         BE    SP650                                                            
         GOTO1 REPORT                                                           
         SPACE 1                                                                
* PRINT THE MARKET GROUP LISTING *                                              
         SPACE 1                                                                
SP650    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R5,15                                                            
         LA    R6,P+19                                                          
         L     R7,VBUFFALO                                                      
         MVI   RCSUBPRG,2                                                       
         XC    SVID(5),SVID                                                     
         XC    BUFREC,BUFREC                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',(R7),BUFREC,0                              
         B     SP670                                                            
         SPACE 1                                                                
SP660    DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'SEQ',(R7),BUFREC,0                               
         SPACE 1                                                                
SP670    DS    0H                                                               
         CLI   DM3,X'80'                                                        
         BE    EXIT                                                             
         SPACE 1                                                                
         CLC   SVID,BUFMGID        SAME MARKET GROUP ID?                        
         BE    SP680                                                            
         MVC   SVID,BUFMGID                                                     
         B     SP690                                                            
         SPACE 1                                                                
SP680    DS    0H                                                               
         CLC   SVMGN,BUFMGN        SAME MARKET GROUP NUMBER?                    
         BE    SP700                                                            
SP690    DS    0H                                                               
         MVC   SVMGN,BUFMGN                                                     
         GOTO1 REPORT                                                           
         MVI   P,0                                                              
         LA    R5,15                                                            
         LA    R6,P2+19                                                         
         MVC   P2+2(1),BUFMGID     PRINT MARKET GROUP ID                        
         MVC   P2+3(4),BUFMGN      PRINT MARKET GROUP NUMBER                    
SP700    DS    0H                                                               
         MVC   0(4,R6),BUFMKT      PRINT MARKET NUMBER                          
         LA    R6,6(R6)                                                         
         BCT   R5,SP660                                                         
         GOTO1 REPORT                                                           
         LA    R5,15                                                            
         LA    R6,P+19                                                          
         B     SP660                                                            
         EJECT                                                                  
* SPMKTNM - GET THE MARKET NAME *                                               
         SPACE 1                                                                
SPMKTNM  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED       MEDIA                                        
         MVC   KEY+2(4),0(R3)      MARKET NUMBER                                
         MVC   KEY+6(2),QAGY       AGENCY                                       
         MVC   KEY+8(9),=9C'0'     FILL                                         
         GOTO1 HIGHMKT                                                          
         B     EXIT                                                             
         SPACE 1                                                                
         USING *,RF                                                             
SP40HDHK DS    0H                                                               
         LM    R9,RB,SP40R9RB                                                   
         B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
* DATA *                                                                        
         SPACE 1                                                                
         DS    0F                                                               
SP40R9RB DS    3F                  HEADHOOK SAVE REGISTER AREA                  
RELO     DS    F                   RELOCATION FACTOR                            
VBUFFALO DS    V                   ADDRESS OF BUFFALO CSECT                     
VMKTTBL  DS    V                   ADDRESS OF MARKET TABLE                      
VMKTTBLX DS    V                   ADDRESS OF END OF MARKET TABLE               
MKTCTR   DC    PL8'0'              COUNT OF MARKETS                             
         SPACE 1                                                                
SVID     DS    CL1                 SAVE OF MARKET GROUP ID                      
SVMGN    DS    CL4                 SAVE OF MARKET GROUP NUMBER                  
         SPACE 1                                                                
BUFREC   DS    0CL10                                                            
BUFMGID  DS    CL1                 MARKET GROUP ID                              
BUFMGN   DS    CL4                 MARKET GROUP NUMBER                          
BUFMKT   DS    CL4                 MARKET NUMBER                                
         DS    CL1                 SPARE                                        
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         BUFF  LINES=1000,FLAVOR=DATA,KEYLIST=(10,A)                            
         SPACE 2                                                                
MKTTBL   CSECT                                                                  
         DS    30000C                                                           
         SPACE 2                                                                
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENMKG                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029SPREP4002 05/01/02'                                      
         END                                                                    
