*          DATA SET SPREP4712  AT LEVEL 013 AS OF 05/01/02                      
*PHASE SP4712A                                                                  
         TITLE 'SP4712 - MARKET GROUP ACTIVITY LISTING'                         
SP4712   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SP4712,RR=R5                                                   
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         LA    R0,SP47HDHK                                                      
         ST    R0,HEADHOOK                                                      
         STM   R9,RB,SP4712R9                                                   
         SPACE 1                                                                
         CLI   MODE,PROCREC                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
* CONTROL SECTION *                                                             
         SPACE 1                                                                
         L     R8,ADMKTGRP                                                      
         USING MKGRECD,R8                                                       
         BAS   RE,SP100            INITIALIZE MARKET TABLES                     
         CLI   MKGKMID,C'G'        IS THIS A SINGLE CLIENT SCHEME?              
         BL    SPCNTL10             YES.                                        
         CLC   =X'0000',SVMGRKEY+3 ARE WE DEALING WITH AGY-WIDE SCHEME?         
         BNE   SPCNTL10             NO.                                         
         CLC   MKGKEY,SAVMGKEY     IS THIS A CLIENT EXCEPTION SCHEME?           
         BE    SPCNTL10             YES                                         
         SPACE 1                                                                
         BAS   RE,SP150             NO. BUILD A CLIENT EXCEPTION TABLE          
         BAS   RE,SP200            PROCESS MARKET GROUPS                        
         CLI   NOTALL,C'Y'         SHOULD I SKIP THIS SCHEME?                   
         BE    EXIT                 YES.                                        
         BAS   RE,SP300            PROCESS BUYS - AGENCY WIDE                   
         BAS   RE,SP400            PRINT LIST                                   
         B     EXIT                                                             
         SPACE 1                                                                
SPCNTL10 BAS   RE,SP200            PROCESS MARKET GROUPS                        
         CLI   NOTALL,C'Y'         SHOULD I SKIP THIS SCHEME?                   
         BE    EXIT                 YES.                                        
         BAS   RE,SP370            PROCESS BUYS - SINGLE CLIENT                 
         BAS   RE,SP400            PRINT LIST                                   
EXIT     XIT1                                                                   
         DROP  R8                                                               
         EJECT                                                                  
* THIS SECTION INITIALIZES TABLES *                                             
         SPACE 1                                                                
SP100    NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         L     RE,=A(MKTTBL)                                                    
         A     RE,RELO                                                          
         ST    RE,AMKTTBL          ADDRESS OF MARKET TABLE                      
         LA    RF,40                                                            
SP110    XC    0(250,RE),0(RE)                                                  
         LA    RE,250(RE)                                                       
         BCT   RF,SP110                                                         
         MVC   LASTMKT,AMKTTBL                                                  
         MVI   RCSUBPRG,3                                                       
         MVI   NOTALL,C'N'                                                      
         B     EXIT                                                             
         SPACE 2                                                                
* THIS SECTION BUILDS THE CLIENT EXCEPTION TABLE *                              
         SPACE 1                                                                
SP150    NTR1                                                                   
         L     RE,=A(CLTTBL)                                                    
         A     RE,RELO                                                          
         ST    RE,ACLTTBL          ADDRESS OF CLIENT EXCEPTION TABLE            
         LA    RF,300               150 CLIENT EXCEPTIONS ARE ALLOWED           
         XCEF                                                                   
         L     R7,ACLTTBL                                                       
         L     R8,ADMKTGRP                                                      
         MVC   SAVMGKEY(13),0(R8)                                               
         LA    R6,24(R8)           POINT TO THE FIRST ELEMENT                   
SP160    DC    0H'0'                                                            
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             IS THIS THE E-O-R?                           
         BE    SP170                YES.                                        
         CLI   0(R6),2             IS THIS THE 02 ELEMENT?                      
         BNE   SP160                NO.                                         
         GOTO1 CLPACK,DMCB,2(R6),0(R7)                                          
         CLI   DM1,0                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R7,2(R7)                                                         
         B     SP160                                                            
         SPACE 1                                                                
SP170    DC    0H'0'                                                            
         B     EXIT                                                             
         EJECT                                                                  
* THIS SECTION FLAGS ALL MARKETS IN A MARKET GROUP *                            
         SPACE 1                                                                
SP200    NTR1                                                                   
         L     R7,AMKTTBL                                                       
         L     R8,ADMKTGRP                                                      
         USING MKGKEY,R8                                                        
         LA    R2,24(R8)                                                        
         USING MKGEL01,R2                                                       
         CLC   MKGKTYP,=X'0D02'    IS THIS A MKTGRP DEFN RECORD?                
         BNE   SP210                NO. GET ONE                                 
         CLC   MKGKMGRP,=X'0000'   SHOULD I LOOK FOR A 01 ELEMENT YET?          
         BE    SP220                YES.                                        
SP210    XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'     0D02                                         
         MVC   KEY+2(7),MKGKAGMD   A-M/CLT/PID/PGRP/MID                         
         GOTO1 GETMKTGR                                                         
         CLC   KEY(11),MKGKEY                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
SP220    CLI   MKGALLM,C'Y'        IS THIS AN ALL MARKET SCHEME?                
         BNE   SP240                NO. SKIP THIS SCHEME                        
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'     0D82 - PASSIVE KEY                           
         MVC   KEY+2(7),MKGKAGMD   A-M/CLT/PID/PGRP/MID                         
         MVC   KEY+3(2),SVMGRKEY+3 CLT                                          
         GOTO1 HIGH                                                             
SP230    CLC   KEY(9),KEYSAVE      SAME 0D82/A-M/CLT/PID/PGRP/MID?              
         BNE   SP250                                                            
         SR    R3,R3                                                            
         ICM   R3,3,KEY+11         GET THE MARKET NUMBER                        
         AR    R3,R7                                                            
         MVI   0(R3),C'M'          FLAG MARKET AS BEING IN A MKT GROUP          
         C     R3,LASTMKT                                                       
         BL    *+8                                                              
         ST    R3,LASTMKT                                                       
         GOTO1 SEQ                                                              
         B     SP230                                                            
         SPACE 1                                                                
SP240    MVI   NOTALL,C'Y'         SET THE 'NOT ALL MARKETS' FLAG               
SP250    DC    0H'0'                                                            
         B     EXIT                                                             
         DROP  R2                                                               
         DROP  R8                                                               
         EJECT                                                                  
* THIS SECTION FLAGS ALL MARKETS WITH BUYS AS ACTIVE *                          
         SPACE 1                                                                
SP300    NTR1                                                                   
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
         L     R7,AMKTTBL                                                       
         L     R8,ADMKTGRP                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(1),2(R8)        A-M                                          
SP310    DC    0H'0'                                                            
         GOTO1 HIGH                                                             
         CLC   KEY(1),KEYSAVE      SAME A-M?                                    
         BNE   EXIT                 NO. EXIT                                    
         L     R6,ACLTTBL                                                       
SP330    CLI   0(R6),0             END OF CLIENT EXCEPTION TABLE?               
         BE    SP340                YES. ADD TO MARKET TABLE                    
         CLC   BUYKCLT,0(R6)       IS THIS BUY BY 'EXCEPTIONAL' CLIENT?         
         BE    SP350                YES. SKIP IT.                               
         LA    R6,2(R6)                                                         
         B     SP330                                                            
         SPACE 1                                                                
SP340    BAS   RE,SPFLAG           FLAG THE MARKET TABLE                        
         MVC   KEY+6(7),=7X'FF'    SKIP TO THE NEXT MARKET                      
         B     SP310                                                            
         SPACE 1                                                                
SP350    MVC   KEY+3(7),=7X'FF'    SKIP TO THE NEXT CLIENT'S BUYS               
         B     SP310                                                            
         DROP  R2                                                               
         SPACE 2                                                                
* THIS SECTION FLAGS ALL MARKETS WITH BUYS AS ACTIVE - SINGLE CLIENT *          
         SPACE 1                                                                
SP370    NTR1                                                                   
         L     R7,AMKTTBL                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(3),SVMGRKEY+2   A-M/CLT                                      
SP380    DC    0H'0'                                                            
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE      SAME A-M/CLT?                                
         BNE   EXIT                 NO. EXIT                                    
         BAS   RE,SPFLAG           FLAG THE MARKET TABLE                        
         MVC   KEY+6(7),=7X'FF'    SKIP TO THE NEXT MARKET                      
         B     SP380                                                            
         EJECT                                                                  
* THIS SECTION PRINTS A LIST OF ACTIVE MARKETS *                                
*  AND STARS THOSE THAT ARE NOT IN A MKT GROUP *                                
         SPACE 1                                                                
SP400    NTR1                                                                   
         L     R7,AMKTTBL                                                       
         LR    R6,R7                                                            
SP410    LA    R5,P+2                                                           
         LA    R4,10                                                            
SP420    CLI   0(R6),0             IS THIS MKT INACTIVE?                        
         BNE   SP430                NO.                                         
         LA    R6,1(R6)                                                         
         C     R6,LASTMKT          HAVE WE DONE LAST ACTIVE MKT?                
         BNH   SP420                NO.                                         
         B     SP440                                                            
         SPACE 1                                                                
SP430    LR    R2,R6                                                            
         SR    R2,R7                                                            
         CVD   R2,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(4,R5),DUB                                                      
         CLI   0(R6),C'*'                                                       
         BNE   *+10                                                             
         MVC   4(1,R5),0(R6)                                                    
         LA    R5,13(R5)                                                        
         LA    R6,1(R6)                                                         
         C     R6,LASTMKT                                                       
         BH    SP440                                                            
         BCT   R4,SP420                                                         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     SP410                                                            
         SPACE 1                                                                
SP440    DC    0H'0'                                                            
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
* SPFLAG - FLAG THE APPROPRIATE ENTRY IN THE MARKET TABLE *                     
         SPACE 1                                                                
SPFLAG   NTR1                                                                   
         SR    R4,R4                                                            
         ICM   R4,3,KEY+4                                                       
         AR    R4,R7                                                            
         CLI   0(R4),C'B'          IS THIS A BOUGHT MARKET IN A GROUP?          
         BE    SPFXIT                                                           
         CLI   0(R4),C'M'          IS THIS MARKET IN SOME MARKET GROUP?         
         BE    SPF10                                                            
         MVI   0(R4),C'*'           NO. FLAG AS HAVING NO GRP IDENTITY          
         B     *+8                                                              
SPF10    MVI   0(R4),C'B'          FLAG AS BOTH BOUGHT AND IN MKT GROUP         
         C     R4,LASTMKT                                                       
         BNH   SPFXIT                                                           
         ST    R4,LASTMKT                                                       
SPFXIT   DC    0H'0'                                                            
         B     EXIT                                                             
* SPTRACE - TRACE SUBROUTINE *                                                  
         SPACE 1                                                                
SPTRACE  NTR1                                                                   
         GOTO1 HEXOUT,DMCB,KEY,P+10,15,=C'MIX',0                                
         GOTO1 HEXOUT,DMCB,KEYSAVE,P2+10,15,=C'MIX',0                           
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 1                                                                
* SP47HDHK - HEADHOOK ROUTINE *                                                 
         SPACE 1                                                                
         USING *,RF                                                             
SP47HDHK NTR1                                                                   
         LM    R9,RB,SP4712R9                                                   
         DROP  RF                                                               
         MVC   H4+59(12),=CL12'MARKET GROUP'                                    
         MVC   H4+72(1),SVMGRKEY+8                                              
         B     EXIT                                                             
SP4712R9 DC    F'0'                                                             
SP4712RA DC    F'0'                                                             
SP4712RB DC    F'0'                                                             
         EJECT                                                                  
         DS    0F                                                               
ACLTTBL  DS    F                   ADDRESS OF CLIENT EXCEPTION TABLE            
AMKTTBL  DS    F                   ADDRESS OF MARKET TABLE                      
LASTMKT  DS    F                   ADDRESS OF LAST MARKET IN MKTTBL             
RELO     DS    F                   RELOCATION ADDRESS                           
SAVMGKEY DS    CL13                SAVE AREA FOR THE MARKET GROUP KEY           
NOTALL   DS    C                   THE NOT ALL MARKETS FLAG                     
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
MKTTBL   CSECT                                                                  
         DS    10000C              10,000 MARKETS                               
         SPACE 2                                                                
CLTTBL   CSECT                                                                  
         DS    300C                150 CLIENT EXCEPTIONS                        
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         SPACE 1                                                                
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013SPREP4712 05/01/02'                                      
         END                                                                    
