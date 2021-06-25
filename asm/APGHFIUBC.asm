*          DATA SET APGHFIUBC  AT LEVEL 051 AS OF 02/21/03                      
*PHASE ACHFUBCA                                                                 
         TITLE 'CARAT CLIENT FILTER GROUPINGS'                                  
ACHFUBC  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
         USING MAND,RA                                                          
         L     RA,0(,R1)                                                        
                                                                                
         USING ACWORKD,RC                                                       
         L     RC,HOOKAWRK                                                      
                                                                                
         USING R1RECD,R7                                                        
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         EJECT                                                                  
*---------------------------------------------------------------------*         
* PUTHOOK -  MATCH ON FILTER VALUE THEN REPLACE WITH FILTER GROUP     *         
*---------------------------------------------------------------------*         
         USING FGROUP,R4                                                        
HOOK     CLI   HOOKTYPE,CMPTHK     PUTHOOK ?                                    
         BNE   HK200               NO                                           
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         AHI   R7,18                                                            
         LA    R4,F4GROUP          FILTER 4 GROUPING                            
                                                                                
HK110    CLI   0(R4),0             ANY MORE TO CHECK                            
         BE    HK130                                                            
         LA    RE,FGRDATA          POINT TO START OF FILTER VALUES              
         SR    RF,RF                                                            
         IC    RF,FGRLN            LENGTH OF ENTRY                              
         SHI   RF,FGRDATA-FGROUP   RF = NUMBER OF FILTER TO CHECK               
                                                                                
HK115    CLC   0(1,RE),R1CDE1      MATCH ON FILTER                              
         BE    HK120               FOUND SO MOVE IN GROUP NUMBER                
         LA    RE,1(,RE)           NOT FOUND, TRY NEXT FILTER                   
         BCT   RF,HK115                                                         
                                                                                
         IC    RF,FGRLN                                                         
         AR    R4,RF               TRY NEXT ENTRY                               
         B     HK110                                                            
                                                                                
HK120    MVC   R1CDE1(1),FGROUP#   REPLACE FILTER WITH GROUP                    
         B     HK132                                                            
         DROP  R4                                                               
                                                                                
HK130    MVI   R1CDE1,C'0'         UNKNOWN GROUP, DEFAULT                       
HK132    CLI   QOPT3,C' '          WAS A SPECIFIC GROUP REQUESTED               
         BE    HK138                                                            
         CLC   QOPT3,R1CDE1        DOES THIS GROUP MATCH REQUESTED              
         BNE   XITNO               GROUP - IF NOT DROP RECORD                   
         CLI   HOOKNUM,1           F4                                           
         BE    HK138                                                            
         MVI   R1CDE1,C' '                                                      
                                                                                
HK138    DS    0H                                                               
         USING R1RECD,R7                                                        
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         CLI   RBROW1,1                                                         
         BNE   *+8                                                              
         LA    R4,RBCDE4                                                        
         CLI   RBROW1,2                                                         
         BNE   *+8                                                              
         LA    R4,RBCDE3                                                        
         CLI   RBROW1,3                                                         
         BNE   *+8                                                              
         LA    R4,RBCDE2                                                        
         CLI   RBROW1,4                                                         
         BNE   *+8                                                              
         LA    R4,RBCDE2                                                        
                                                                                
         CLI   RBROW1,5                                                         
         BNE   *+8                                                              
         LA    R4,RBCDE4                                                        
         CLI   RBROW1,6                                                         
         BNE   *+8                                                              
         LA    R4,RBCDE3                                                        
         CLI   RBROW1,7                                                         
         BNE   *+8                                                              
         LA    R4,RBCDE2                                                        
         CLI   RBROW1,8                                                         
         BNE   *+8                                                              
         LA    R4,RBCDE2                                                        
                                                                                
         CLI   RBROW1,4                                                         
         BH    HK180                                                            
         SP    RBCOL4,RBCOL2                                                    
         SP    RBCOL5,RBCOL3                                                    
         SP    RBCOL9,RBCOL7                                                    
         SP    RBCOL10,RBCOL8                                                   
         B     HK190                                                            
                                                                                
HK180    EQU   *                                                                
         SP    RBCOL6,RBCOL5                                                    
         SP    RBCOL3,RBCOL2                                                    
                                                                                
HK190    EQU   *                                                                
         CLI   QOPT6,C'1'                                                       
         BNE   XIT                                                              
         MVC   DMPBLRB,=CL6'MODE1B'                                             
         BAS   RE,DMPREC                                                        
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*  SORTOUT - REPLACE ROW 1 WITH GROUP NAME                          *           
*-------------------------------------------------------------------*           
HK200    CLI   HOOKTYPE,CMSROT     SORTOUT ?                                    
         BNE   HK300                                                            
         CLI   HOOKNUM,1                                                        
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
*        CLC   R1SAVKEY,SPACES       FIRST TIME IN?                             
*        BE    HK204                                                            
*                                                                               
*        CLC   R1CDE1SV,R1CDE1                                                  
*        BNE   HK202                                                            
*        CLI   R1REPNO,4             CHECK FOR KEY BREAKS - IF                  
*        BE    HK204                 YES, ACCUMULATORS NEED TO BE               
*        CLI   R1REPNO,8             CLEARED                                    
*        BE    HK204                                                            
*        CLI   R1REPNO,3                                                        
*        BE    HK204                                                            
*        CLI   R1REPNO,7                                                        
*        BE    HK204                                                            
*                                                                               
*        CLC   R1CDE3SV,R1CDE3                                                  
*        BNE   HK202                                                            
*        CLI   R1REPNO,2                                                        
*        BE    HK204                                                            
*        CLI   R1REPNO,6                                                        
*        BE    HK204                                                            
*                                                                               
*        CLC   R1CDE2SV,R1CDE2                                                  
*        BNE   HK202                                                            
*        CLI   R1REPNO,1                                                        
*        BE    HK204                                                            
*        CLI   R1REPNO,5                                                        
*        BE    HK204                                                            
*                                                                               
*K202    BAS   RE,CLRACCUM           CLEAR THE ENTIRE ACCUM TABLE               
*                                                                               
*K204    EQU   *                                                                
*        MVC   R1SAVKEY,R1RECD       SET SAVE KEY FOR NEXT PASS                 
*                                                                               
*        CLI   QOPT6,C'2'            RECORD DUMP                                
*        BNE   *+14                                                             
*        MVC   DMPBLRB,=CL6'MODE2B'                                             
*        BAS   RE,DMPREC                                                        
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        ACCOUNT FOR THE TRANSFER BUG THAT LEFT A FEW CENTS HERE    *           
*        AND THERE IN THE INCOME ACCOUNTS THAT SHOULD HAVE BEEN     *           
*        ZERO, HENCE CREATING VERY LARGE PERCENTAGE CALCULATIONS    *           
*-------------------------------------------------------------------*           
         CLI   R1REPNO,4                                                        
         BH    HK205                                                            
                                                                                
         CP    R1COL1,=P'99'                                                    
         BH    *+20                                                             
         CP    R1COL1,=P'-99'                                                   
         BL    *+10                                                             
         ZAP   R1COL1,=P'0'                                                     
                                                                                
         CP    R1COL3,=P'99'                                                    
         BH    *+20                                                             
         CP    R1COL3,=P'-99'                                                   
         BL    *+10                                                             
         ZAP   R1COL3,=P'0'                                                     
                                                                                
         CP    R1COL4,=P'99'                                                    
         BH    *+20                                                             
         CP    R1COL4,=P'-99'                                                   
         BL    *+10                                                             
         ZAP   R1COL4,=P'0'                                                     
                                                                                
         CP    R1COL5,=P'99'                                                    
         BH    *+20                                                             
         CP    R1COL5,=P'-99'                                                   
         BL    *+10                                                             
         ZAP   R1COL5,=P'0'                                                     
                                                                                
         CP    R1COL6,=P'99'                                                    
         BH    *+20                                                             
         CP    R1COL6,=P'-99'                                                   
         BL    *+10                                                             
         ZAP   R1COL6,=P'0'                                                     
                                                                                
         CP    R1COL8,=P'99'                                                    
         BH    *+20                                                             
         CP    R1COL8,=P'-99'                                                   
         BL    *+10                                                             
         ZAP   R1COL8,=P'0'                                                     
                                                                                
         CP    R1COL9,=P'99'                                                    
         BH    *+20                                                             
         CP    R1COL9,=P'-99'                                                   
         BL    *+10                                                             
         ZAP   R1COL9,=P'0'                                                     
                                                                                
         CP    R1COL10,=P'99'                                                   
         BH    *+20                                                             
         CP    R1COL10,=P'-99'                                                  
         BL    *+10                                                             
         ZAP   R1COL10,=P'0'                                                    
         B     HK280                                                            
                                                                                
HK205    EQU   *                                                                
         CP    R1COL1,=P'99'                                                    
         BH    *+20                                                             
         CP    R1COL1,=P'-99'                                                   
         BL    *+10                                                             
         ZAP   R1COL1,=P'0'                                                     
                                                                                
         CP    R1COL2,=P'99'                                                    
         BH    *+20                                                             
         CP    R1COL2,=P'-99'                                                   
         BL    *+10                                                             
         ZAP   R1COL2,=P'0'                                                     
                                                                                
         CP    R1COL3,=P'99'                                                    
         BH    *+20                                                             
         CP    R1COL3,=P'-99'                                                   
         BL    *+10                                                             
         ZAP   R1COL3,=P'0'                                                     
                                                                                
         CP    R1COL4,=P'99'                                                    
         BH    *+20                                                             
         CP    R1COL4,=P'-99'                                                   
         BL    *+10                                                             
         ZAP   R1COL4,=P'0'                                                     
                                                                                
         CP    R1COL5,=P'99'                                                    
         BH    *+20                                                             
         CP    R1COL5,=P'-99'                                                   
         BL    *+10                                                             
         ZAP   R1COL5,=P'0'                                                     
                                                                                
         CP    R1COL6,=P'99'                                                    
         BH    *+20                                                             
         CP    R1COL6,=P'-99'                                                   
         BL    *+10                                                             
         ZAP   R1COL6,=P'0'                                                     
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        KEY COL STATEMENTS ARE CAUSING THE PERCENTAGES AND OTHER   *           
*        SUPERLEDGER CALCULATED ACCOUNTS TO NOT AGREE WITH OTHER    *           
*        SECTIONS OF THE REPORT, SO HERE WE ARE TOTALLING UP MUCH   *           
*        OF THE DOLLARS IN THE HOOK                                 *           
*-------------------------------------------------------------------*           
         USING R1RECD,R7                                                        
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         CLI   R1REPNO,1                                                        
         BNE   *+8                                                              
         LA    R4,R1CDE4                                                        
         CLI   R1REPNO,2                                                        
         BNE   *+8                                                              
         LA    R4,R1CDE3                                                        
         CLI   R1REPNO,3                                                        
         BNE   *+8                                                              
         LA    R4,R1CDE2                                                        
         CLI   R1REPNO,4                                                        
         BNE   *+8                                                              
         LA    R4,R1CDE2                                                        
                                                                                
         CLI   R1REPNO,5                                                        
         BNE   *+8                                                              
         LA    R4,R1CDE4                                                        
         CLI   R1REPNO,6                                                        
         BNE   *+8                                                              
         LA    R4,R1CDE3                                                        
         CLI   R1REPNO,7                                                        
         BNE   *+8                                                              
         LA    R4,R1CDE2                                                        
         CLI   R1REPNO,8                                                        
         BNE   *+8                                                              
         LA    R4,R1CDE2                                                        
                                                                                
         CLC   0(2,R4),=C'21'                                                   
         BNE   HK280                                                            
                                                                                
         LA    R6,R1CDE5                                                        
         CLI   R1REPNO,1                                                        
         BE    HK206                                                            
         LA    R6,R1CDE4                                                        
         CLI   R1REPNO,2                                                        
         BE    HK206                                                            
         LA    R6,R1CDE3                                                        
         CLI   R1REPNO,3                                                        
         BE    HK206                                                            
         LA    R6,R1CDE3                                                        
         CLI   R1REPNO,4                                                        
         BE    HK206                                                            
         LA    R6,R1CDE5                                                        
         CLI   R1REPNO,5                                                        
         BE    HK206                                                            
         LA    R6,R1CDE4                                                        
         CLI   R1REPNO,6                                                        
         BE    HK206                                                            
         LA    R6,R1CDE3                                                        
         CLI   R1REPNO,7                                                        
         BE    HK206                                                            
         LA    R6,R1CDE3                                                        
         CLI   R1REPNO,8                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
HK206    CLI   13(R6),X'41'                                                     
         BNE   HK207                                                            
         MVC   R1SAVKEY,R1RECD       SET SAVE KEY FOR NEXT PASS                 
         LA    R1,6                                                             
         CLI   R1REPNO,4                                                        
         BH    *+8                                                              
         LA    R1,10                                                            
         LA    R5,FAA21NEG                                                      
         LA    R6,R1COL1                                                        
HK206B   ZAP   0(8,R5),0(8,R6)                                                  
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         BCT   R1,HK206B                                                        
         B     HK208                                                            
                                                                                
HK207    CLI   13(R6),X'C2'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   13(R6),X'41'                                                     
         CLC   R1SAVKEY,R1RECD                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   13(R6),X'C2'                                                     
         LA    R5,FAA21NEG                                                      
         LA    R6,R1COL1                                                        
         LA    R1,6                                                             
         CLI   R1REPNO,4                                                        
         BH    *+8                                                              
         LA    R1,10                                                            
HK207B   CP    0(8,R5),=P'0'                                                    
         BH    HK207E                                                           
*MN      CP    0(8,R6),=P'0'                                                    
*MN      BH    HK207E                                                           
*MN      MP    0(8,R6),=P'-1'                                                   
         ZAP   0(8,R6),=P'0'                                                    
HK207E   LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         BCT   R1,HK207B                                                        
                                                                                
HK208    EQU   *                                                                
                                                                                
*        CLI   R1REPNO,4                                                        
*        BH    HK240                                                            
*        CLI   R1REPNO,1                                                        
*        BNE   *+8                                                              
*        LA    R4,R1CDE4                                                        
*        CLI   R1REPNO,2                                                        
*        BNE   *+8                                                              
*        LA    R4,R1CDE3                                                        
*        CLI   R1REPNO,3                                                        
*        BNE   *+8                                                              
*        LA    R4,R1CDE2                                                        
*        CLI   R1REPNO,4                                                        
*        BNE   *+8                                                              
*        LA    R4,R1CDE2                                                        
*                                                                               
*        LA    R5,FAA01                                                         
*        CLC   0(2,R4),=C'01'                                                   
*        BE    HK210                                                            
*        LA    R5,FAA03                                                         
*        CLC   0(2,R4),=C'03'                                                   
*        BE    HK210                                                            
*        LA    R5,FAA07                                                         
*        CLC   0(2,R4),=C'07'                                                   
*        BE    HK210                                                            
*        LA    R5,FAA08                                                         
*        CLC   0(2,R4),=C'08'                                                   
*        BE    HK210                                                            
*        LA    R5,FAA10                                                         
*        CLC   0(2,R4),=C'10'                                                   
*        BE    HK210                                                            
*        LA    R5,FAA17                                                         
*        CLC   0(2,R4),=C'17'                                                   
*        BE    HK210                                                            
*        B     HK215                                                            
*                                                                               
*K210    EQU   *                                                                
*        CP    R1COL1,=P'0'                                                     
*        BNE   HK212                                                            
*        CP    R1COL2,=P'0'                                                     
*        BNE   HK212                                                            
*        CP    R1COL6,=P'0'                                                     
*        BNE   HK212                                                            
*        CP    R1COL7,=P'0'                                                     
*        BE    HK280                                                            
*                                                                               
*K212    LA    R6,R1COL1                                                        
*        LA    R1,10                                                            
*K213    AP    0(8,R5),0(8,R6)                                                  
*        LA    R5,8(R5)                                                         
*        LA    R6,8(R6)                                                         
*        BCT   R1,HK213                                                         
*        B     HK280                                                            
*                                                                               
*K215    EQU   *                                                                
*        CLC   0(2,R4),=C'05'                                                   
*        BNE   HK218                                                            
*                                                                               
*        LA    R6,R1CDE5                                                        
*        CLI   R1REPNO,1                                                        
*        BE    HK216                                                            
*        LA    R6,R1CDE4                                                        
*        CLI   R1REPNO,2                                                        
*        BE    HK216                                                            
*        LA    R6,R1CDE3                                                        
*        CLI   R1REPNO,3                                                        
*        BE    HK216                                                            
*        LA    R6,R1CDE3                                                        
*        CLI   R1REPNO,4                                                        
*        BE    HK216                                                            
*                                                                               
*K216    EQU   *                                                                
*        LA    R5,FAA01                                                         
*        CLI   13(R6),X'41'                                                     
*        BNE   *+8                                                              
*        LA    R5,FAA03                                                         
*        LA    R6,R1COL1                                                        
*        LA    R1,10                                                            
*K217    ZAP   0(8,R6),0(8,R5)                                                  
*        LA    R5,8(R5)                                                         
*        LA    R6,8(R6)                                                         
*        BCT   R1,HK217                                                         
*        B     HK280                                                            
*                                                                               
*K218    EQU   *                                                                
*        CLC   0(2,R4),=C'09'                                                   
*        BNE   HK220                                                            
*        LA    R5,FAA07                                                         
*        LA    R6,R1COL1                                                        
*        LA    R1,10                                                            
*K219    ZAP   0(8,R6),0(8,R5)                                                  
*        LA    R5,8(R5)                                                         
*        LA    R6,8(R6)                                                         
*        BCT   R1,HK219                                                         
*                                                                               
*        LA    R5,FAA08                                                         
*        LA    R6,R1COL1                                                        
*        LA    R1,10                                                            
*K219A   AP    0(8,R6),0(8,R5)                                                  
*        LA    R5,8(R5)                                                         
*        LA    R6,8(R6)                                                         
*        BCT   R1,HK219A                                                        
*        B     HK280                                                            
*                                                                               
*K220    EQU   *                                                                
*        CLC   0(2,R4),=C'19'                                                   
*        BNE   HK225                                                            
*        BAS   RE,TOTFAA19                                                      
*        LA    R5,FAA19                                                         
*        LA    R6,R1COL1                                                        
*        LA    R1,10                                                            
*K222    ZAP   0(8,R6),0(8,R5)                                                  
*        LA    R5,8(R5)                                                         
*        LA    R6,8(R6)                                                         
*        BCT   R1,HK222                                                         
*        B     HK280                                                            
*                                                                               
*K225    EQU   *                                                                
*        CLC   0(2,R4),=C'21'                                                   
*        BNE   HK280                                                            
*                                                                               
*        LA    R6,R1CDE5                                                        
*        CLI   R1REPNO,1                                                        
*        BE    HK228                                                            
*        LA    R6,R1CDE4                                                        
*        CLI   R1REPNO,2                                                        
*        BE    HK228                                                            
*        LA    R6,R1CDE3                                                        
*        CLI   R1REPNO,3                                                        
*        BE    HK228                                                            
*        LA    R6,R1CDE3                                                        
*        CLI   R1REPNO,4                                                        
*        BE    HK228                                                            
*                                                                               
*K228    EQU   *                                                                
*        LA    R5,FAA03                                                         
*        CLI   13(R6),X'41'                                                     
*        BNE   *+12                                                             
*        BAS   RE,TOTFAA21                                                      
*        LA    R5,FAA21                                                         
*        LA    R6,R1COL1                                                        
*        LA    R1,10                                                            
*K230    ZAP   0(8,R6),0(8,R5)                                                  
*        LA    R5,8(R5)                                                         
*        LA    R6,8(R6)                                                         
*        BCT   R1,HK230                                                         
*        B     HK280                                                            
*        EJECT                                                                  
*-------------------------------------------------------------------*           
*        KEY COL STATEMENTS ARE CAUSING THE PERCENTAGES AND OTHER   *           
*        SUPERLEDGER CALCULATED ACCOUNTS TO NOT AGREE WITH OTHER    *           
*        SECTIONS OF THE REPORT, SO HERE WE ARE TOTALLING UP MUCH   *           
*        OF THE DOLLARS IN THE HOOK                                 *           
*-------------------------------------------------------------------*           
*K240    EQU   *                                                                
*        CLI   R1REPNO,5                                                        
*        BNE   *+8                                                              
*        LA    R4,R1CDE4                                                        
*        CLI   R1REPNO,6                                                        
*        BNE   *+8                                                              
*        LA    R4,R1CDE3                                                        
*        CLI   R1REPNO,7                                                        
*        BNE   *+8                                                              
*        LA    R4,R1CDE2                                                        
*        CLI   R1REPNO,8                                                        
*        BNE   *+8                                                              
*        LA    R4,R1CDE2                                                        
*                                                                               
*        LA    R5,FAA01                                                         
*        CLC   0(2,R4),=C'01'                                                   
*        BE    HK245                                                            
*        LA    R5,FAA03                                                         
*        CLC   0(2,R4),=C'03'                                                   
*        BE    HK245                                                            
*        LA    R5,FAA07                                                         
*        CLC   0(2,R4),=C'07'                                                   
*        BE    HK245                                                            
*        LA    R5,FAA08                                                         
*        CLC   0(2,R4),=C'08'                                                   
*        BE    HK245                                                            
*        LA    R5,FAA10                                                         
*        CLC   0(2,R4),=C'10'                                                   
*        BE    HK245                                                            
*        LA    R5,FAA17                                                         
*        CLC   0(2,R4),=C'17'                                                   
*        BE    HK245                                                            
*        B     HK250                                                            
*                                                                               
*K245    EQU   *                                                                
*        CP    R1COL1,=P'0'                                                     
*        BNE   HK247                                                            
*        CP    R1COL4,=P'0'                                                     
*        BE    HK280                                                            
*K247    LA    R6,R1COL1                                                        
*        LA    R1,6                                                             
*K248    AP    0(8,R5),0(8,R6)                                                  
*        LA    R5,8(R5)                                                         
*        LA    R6,8(R6)                                                         
*        BCT   R1,HK248                                                         
*        B     HK280                                                            
*                                                                               
*K250    EQU   *                                                                
*        CLC   0(2,R4),=C'05'                                                   
*        BNE   HK260                                                            
*                                                                               
*        LA    R6,R1CDE5                                                        
*        CLI   R1REPNO,5                                                        
*        BE    HK255                                                            
*        LA    R6,R1CDE4                                                        
*        CLI   R1REPNO,6                                                        
*        BE    HK255                                                            
*        LA    R6,R1CDE3                                                        
*        CLI   R1REPNO,7                                                        
*        BE    HK255                                                            
*        LA    R6,R1CDE3                                                        
*        CLI   R1REPNO,8                                                        
*        BE    HK255                                                            
*                                                                               
*K255    EQU   *                                                                
*        LA    R5,FAA01                                                         
*        CLI   13(R6),X'41'                                                     
*        BNE   *+8                                                              
*        LA    R5,FAA03                                                         
*        LA    R6,R1COL1                                                        
*        LA    R1,6                                                             
*K258    ZAP   0(8,R6),0(8,R5)                                                  
*        LA    R5,8(R5)                                                         
*        LA    R6,8(R6)                                                         
*        BCT   R1,HK258                                                         
*        B     HK280                                                            
*                                                                               
*K260    EQU   *                                                                
*        CLC   0(2,R4),=C'09'                                                   
*        BNE   HK265                                                            
*        LA    R5,FAA07                                                         
*        LA    R6,R1COL1                                                        
*        LA    R1,6                                                             
*K261    ZAP   0(8,R6),0(8,R5)                                                  
*        LA    R5,8(R5)                                                         
*        LA    R6,8(R6)                                                         
*        BCT   R1,HK261                                                         
*                                                                               
*        LA    R5,FAA08                                                         
*        LA    R6,R1COL1                                                        
*        LA    R1,6                                                             
*K262    AP    0(8,R6),0(8,R5)                                                  
*        LA    R5,8(R5)                                                         
*        LA    R6,8(R6)                                                         
*        BCT   R1,HK262                                                         
*        B     HK280                                                            
*                                                                               
*K265    EQU   *                                                                
*        CLC   0(2,R4),=C'19'                                                   
*        BNE   HK270                                                            
*        BAS   RE,TOTFAA19                                                      
*        LA    R5,FAA19                                                         
*        LA    R6,R1COL1                                                        
*        LA    R1,6                                                             
*K268    ZAP   0(8,R6),0(8,R5)                                                  
*        LA    R5,8(R5)                                                         
*        LA    R6,8(R6)                                                         
*        BCT   R1,HK268                                                         
*        B     HK280                                                            
*                                                                               
*K270    EQU   *                                                                
*        CLC   0(2,R4),=C'21'                                                   
*        BNE   HK280                                                            
*                                                                               
*        LA    R6,R1CDE5                                                        
*        CLI   R1REPNO,5                                                        
*        BE    HK275                                                            
*        LA    R6,R1CDE4                                                        
*        CLI   R1REPNO,6                                                        
*        BE    HK275                                                            
*        LA    R6,R1CDE3                                                        
*        CLI   R1REPNO,7                                                        
*        BE    HK275                                                            
*        LA    R6,R1CDE3                                                        
*        CLI   R1REPNO,8                                                        
*        BE    HK275                                                            
*                                                                               
*K275    EQU   *                                                                
*        LA    R5,FAA03                                                         
*        CLI   13(R6),X'41'                                                     
*        BNE   *+12                                                             
*        BAS   RE,TOTFAA21                                                      
*        LA    R5,FAA21                                                         
*        LA    R6,R1COL1                                                        
*        LA    R1,6                                                             
*K278    ZAP   0(8,R6),0(8,R5)                                                  
*        LA    R5,8(R5)                                                         
*        LA    R6,8(R6)                                                         
*        BCT   R1,HK278                                                         
*        B     HK280                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        FIND FILTER GROUP NAME IN TABLE AND PUT IT IN SORT REC     *           
*-------------------------------------------------------------------*           
HK280    LA    R4,GRPTAB                                                        
HK285    CLI   0(R4),X'FF'         EOT                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R4),R1CDE1      MATCH ON GROUP VALUE                         
         BE    HK290                                                            
         LA    R4,GRPLEN(,R4)                                                   
         B     HK285                                                            
HK290    MVC   R1NME1,1(R4)        REPLACE WITH GROUP NAME                      
                                                                                
         CLI   QOPT6,C'2'                                                       
         BNE   *+14                                                             
         MVC   DMPBLRB,=CL6'MODE2A'                                             
         BAS   RE,DMPREC                                                        
                                                                                
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------*              
*        SORTHOOK                                                               
*----------------------------------------------------------------*              
HK300    CLI   HOOKTYPE,CMSRHK                                                  
         BNE   XIT                                                              
         CLI   HOOKNUM,1                                                        
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
HK305    MVI   R1CDE1,C'1'                                                      
                                                                                
XIT      SR    RC,RC                                                            
XITNO    LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------*              
*        CLEAR THE ACCUMULATOR TABLE                                            
*----------------------------------------------------------------*              
*CLRACCUM NTR1                                                                  
*         LA    R5,FAAAMTS                                                      
*CLR100   CLI   0(R5),X'FF'                                                     
*         BE    CLRXIT                                                          
*         ZAP   0(8,R5),=P'0'                                                   
*         LA    R5,8(R5)                                                        
*         B     CLR100                                                          
*CLRXIT   XIT1                                                                  
*         EJECT                                                                 
*----------------------------------------------------------------*              
*        ACCUMULATE TOTAL FOR PROFIT PERCENT CALCULATION                        
*----------------------------------------------------------------*              
*TOTFAA21 NTR1                                                                  
*         LA    R2,FAA21                                                        
*         LA    R3,FAA03                                                        
*         LA    R4,10                                                           
*TOT21A   ZAP   0(8,R2),0(8,R3)                                                 
*         LA    R2,8(R2)                                                        
*         LA    R3,8(R3)                                                        
*         BCT   R4,TOT21A                                                       
*                                                                               
*         LA    R2,FAA21                                                        
*         LA    R3,FAA07                                                        
*         LA    R4,10                                                           
*TOT21B   SP    0(8,R2),0(8,R3)                                                 
*         LA    R2,8(R2)                                                        
*         LA    R3,8(R3)                                                        
*         BCT   R4,TOT21B                                                       
*                                                                               
*         LA    R2,FAA21                                                        
*         LA    R3,FAA08                                                        
*         LA    R4,10                                                           
*TOT21C   SP    0(8,R2),0(8,R3)                                                 
*         LA    R2,8(R2)                                                        
*         LA    R3,8(R3)                                                        
*         BCT   R4,TOT21C                                                       
*                                                                               
*         LA    R2,FAA21                                                        
*         LA    R3,FAA10                                                        
*         LA    R4,10                                                           
*TOT21D   SP    0(8,R2),0(8,R3)                                                 
*         LA    R2,8(R2)                                                        
*         LA    R3,8(R3)                                                        
*         BCT   R4,TOT21D                                                       
*                                                                               
*         LA    R2,FAA21                                                        
*         LA    R3,FAA17                                                        
*         LA    R4,10                                                           
*TOT21E   SP    0(8,R2),0(8,R3)                                                 
*         LA    R2,8(R2)                                                        
*         LA    R3,8(R3)                                                        
*         BCT   R4,TOT21E                                                       
*                                                                               
*         XIT1                                                                  
*         EJECT                                                                 
*----------------------------------------------------------------*              
*        ACCUMULATE TOTAL FOR NET MARGIN ACCOUNT                                
*----------------------------------------------------------------*              
*TOTFAA19 NTR1                                                                  
*         LA    R2,FAA19                                                        
*         LA    R3,FAA03                                                        
*         LA    R4,10                                                           
*TOT19A   ZAP   0(8,R2),0(8,R3)                                                 
*         LA    R2,8(R2)                                                        
*         LA    R3,8(R3)                                                        
*         BCT   R4,TOT19A                                                       
*                                                                               
*         LA    R2,FAA19                                                        
*         LA    R3,FAA07                                                        
*         LA    R4,10                                                           
*TOT19B   SP    0(8,R2),0(8,R3)                                                 
*         LA    R2,8(R2)                                                        
*         LA    R3,8(R3)                                                        
*         BCT   R4,TOT19B                                                       
*                                                                               
*         LA    R2,FAA19                                                        
*         LA    R3,FAA08                                                        
*         LA    R4,10                                                           
*TOT19C   SP    0(8,R2),0(8,R3)                                                 
*         LA    R2,8(R2)                                                        
*         LA    R3,8(R3)                                                        
*         BCT   R4,TOT19C                                                       
*                                                                               
*         LA    R2,FAA19                                                        
*         LA    R3,FAA10                                                        
*         LA    R4,10                                                           
*TOT19D   SP    0(8,R2),0(8,R3)                                                 
*         LA    R2,8(R2)                                                        
*         LA    R3,8(R3)                                                        
*         BCT   R4,TOT19D                                                       
*                                                                               
*         LA    R2,FAA19                                                        
*         LA    R3,FAA17                                                        
*         LA    R4,10                                                           
*TOT19E   SP    0(8,R2),0(8,R3)                                                 
*         LA    R2,8(R2)                                                        
*         LA    R3,8(R3)                                                        
*         BCT   R4,TOT19E                                                       
*                                                                               
*         XIT1                                                                  
         EJECT                                                                  
*----------------------------------------------------------------*              
*        DUMP SELECT RECORDS                                                    
*----------------------------------------------------------------*              
DMPREC   NTR1                                                                   
         CLI   R1REPNO,1                                                        
         BNE   *+8                                                              
         LA    R4,R1CDE4                                                        
         CLI   R1REPNO,2                                                        
         BNE   *+8                                                              
         LA    R4,R1CDE3                                                        
         CLI   R1REPNO,3                                                        
         BNE   *+8                                                              
         LA    R4,R1CDE2                                                        
         CLI   R1REPNO,4                                                        
         BNE   *+8                                                              
         LA    R4,R1CDE2                                                        
                                                                                
         CLI   R1REPNO,5                                                        
         BNE   *+8                                                              
         LA    R4,R1CDE4                                                        
         CLI   R1REPNO,6                                                        
         BNE   *+8                                                              
         LA    R4,R1CDE3                                                        
         CLI   R1REPNO,7                                                        
         BNE   *+8                                                              
         LA    R4,R1CDE2                                                        
         CLI   R1REPNO,8                                                        
         BNE   *+8                                                              
         LA    R4,R1CDE2                                                        
                                                                                
*        CLC   0(2,R4),=C'01'                                                   
*        BE    DMPR10                                                           
*        CLC   0(2,R4),=C'03'                                                   
*        BE    DMPR10                                                           
*        CLC   0(2,R4),=C'05'                                                   
*        BE    DMPR10                                                           
*        CLC   0(2,R4),=C'07'                                                   
*        BE    DMPR10                                                           
*        CLC   0(2,R4),=C'08'                                                   
*        BE    DMPR10                                                           
*        CLC   0(2,R4),=C'10'                                                   
*        BE    DMPR10                                                           
*        CLC   0(2,R4),=C'17'                                                   
*        BE    DMPR10                                                           
         CLC   0(2,R4),=C'21'                                                   
         BE    DMPR10                                                           
         B     DMPXIT                                                           
                                                                                
DMPR10   L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         CLI   QOPT6,C'2'                                                       
         BE    *+8                                                              
         AHI   R0,18                                                            
         GOTO1 PRNTBL,DMCB,(6,DMPBLRB),(R7),C'DUMP',(R0),=C'2D',       X        
               (C'P',PRINT)                                                     
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         CLI   QOPT6,C'2'                                                       
         BE    *+8                                                              
         AHI   R7,18                                                            
                                                                                
DMPXIT   XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------*              
*        DEFINE CONSTANTS                                                       
*----------------------------------------------------------------*              
*        FILTER 4 GROUPS                                                        
F4GROUP  DS    0C                                                               
F4GRP1   DC    C'1',AL1(F4GRP1X-F4GRP1),C'AEMP'                                 
F4GRP1X  EQU   *                                                                
                                                                                
F4GRP2   DC    C'2',AL1(F4GRP2X-F4GRP2),C'CGK'                                  
F4GRP2X  EQU   *                                                                
                                                                                
F4GRP3   DC    C'3',AL1(F4GRP3X-F4GRP3),C'I'                                    
F4GRP3X  EQU   *                                                                
                                                                                
F4GRP4   DC    C'4',AL1(F4GRP4X-F4GRP4),C'S'                                    
F4GRP4X  EQU   *                                                                
         DC    X'00'                                                            
                                                                                
*        GROUP NAMES                                                            
GRPTAB   DS    0C                                                               
         DC    CL1'0',CL36'NOT-GROUPED'                                         
GRPLEN   EQU   *-GRPTAB                                                         
         DC    CL1'1',CL36'RONNIE BEASON'                                       
         DC    CL1'2',CL36'PAT DORNER'                                          
         DC    CL1'3',CL36'MISCELLANEOUS'                                       
         DC    CL1'4',CL36'SHARED RESOURCES'                                    
         DC    X'FF'                                                            
                                                                                
DMPBLRB  DC    CL6'RECBEF'                                                      
                                                                                
R1SAVKEY DC    CL(R1SAVLEN)' '                                                  
         ORG   R1SAVKEY                                                         
R1ROW1SV DS    XL2                                                              
R1CDE1SV DS    XL14                                                             
R1ROW2SV DS    XL2                                                              
R1CDE2SV DS    XL14                                                             
R1ROW3SV DS    XL2                                                              
R1CDE3SV DS    XL14                                                             
R1ROW4SV DS    XL2                                                              
R1CDE4SV DS    XL14                                                             
R1ROW5SV DS    XL2                                                              
R1CDE5SV DS    XL14                                                             
         ORG                                                                    
                                                                                
FAAAMTS  DS    0F                                                               
FAA01    DC    PL8'0',PL8'0',PL8'0',PL8'0',PL8'0',PL8'0'                        
         DC    PL8'0',PL8'0',PL8'0',PL8'0'                                      
FAA03    DC    PL8'0',PL8'0',PL8'0',PL8'0',PL8'0',PL8'0'                        
         DC    PL8'0',PL8'0',PL8'0',PL8'0'                                      
FAA07    DC    PL8'0',PL8'0',PL8'0',PL8'0',PL8'0',PL8'0'                        
         DC    PL8'0',PL8'0',PL8'0',PL8'0'                                      
FAA08    DC    PL8'0',PL8'0',PL8'0',PL8'0',PL8'0',PL8'0'                        
         DC    PL8'0',PL8'0',PL8'0',PL8'0'                                      
FAA10    DC    PL8'0',PL8'0',PL8'0',PL8'0',PL8'0',PL8'0'                        
         DC    PL8'0',PL8'0',PL8'0',PL8'0'                                      
FAA17    DC    PL8'0',PL8'0',PL8'0',PL8'0',PL8'0',PL8'0'                        
         DC    PL8'0',PL8'0',PL8'0',PL8'0'                                      
FAA19    DC    PL8'0',PL8'0',PL8'0',PL8'0',PL8'0',PL8'0'                        
         DC    PL8'0',PL8'0',PL8'0',PL8'0'                                      
FAA21    DC    PL8'0',PL8'0',PL8'0',PL8'0',PL8'0',PL8'0'                        
         DC    PL8'0',PL8'0',PL8'0',PL8'0'                                      
         DC    X'FF'                                                            
                                                                                
FAA21NEG DC    PL8'0',PL8'0',PL8'0',PL8'0',PL8'0',PL8'0'                        
         DC    PL8'0',PL8'0',PL8'0',PL8'0'                                      
         DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------*              
*        WORKING STORAGE                                                        
*----------------------------------------------------------------*              
RBRECD   DSECT                                                                  
RBHEAD   DS    XL18                                                             
RBROW1   DS    XL2                                                              
RBCDE1   DS    XL14                                                             
RBROW2   DS    XL2                                                              
RBCDE2   DS    XL14                                                             
RBROW3   DS    XL2                                                              
RBCDE3   DS    XL14                                                             
RBROW4   DS    XL2                                                              
RBCDE4   DS    XL14                                                             
RBROW5   DS    XL2                                                              
RBCDE5   DS    XL14                                                             
                                                                                
RBNA     DS    CL80                                                             
                                                                                
RBCOL1   DS    PL8                                                              
RBCOL2   DS    PL8                                                              
RBCOL3   DS    PL8                                                              
RBCOL4   DS    PL8                                                              
RBCOL5   DS    PL8                                                              
RBCOL6   DS    PL8                                                              
RBCOL7   DS    PL8                                                              
RBCOL8   DS    PL8                                                              
RBCOL9   DS    PL8                                                              
RBCOL10  DS    PL8                                                              
                                                                                
RBNME1   DS    CL36                                                             
RBNME2   DS    CL36                                                             
RBNME3   DS    CL36                                                             
RBNME4   DS    CL36                                                             
RBNME5   DS    CL36                                                             
RBLEN    EQU   *-RBRECD                                                         
                                                                                
         ORG   RBRECD                                                           
R1RECD   DS    0C                                                               
R1ROW1   DS    XL2                                                              
R1CDE1   DS    XL14                                                             
R1ROW2   DS    XL2                                                              
R1CDE2   DS    XL14                                                             
R1ROW3   DS    XL2                                                              
R1CDE3   DS    XL14                                                             
R1ROW4   DS    XL2                                                              
R1CDE4   DS    XL14                                                             
R1ROW5   DS    XL2                                                              
R1CDE5   DS    XL14                                                             
R1SAVLEN EQU   *-R1RECD                                                         
R1REPNO  DS    XL1                                                              
R1REPCP  DS    XL1                                                              
R1TYPE   DS    XL2                                                              
                                                                                
R1NME1   DS    CL36                                                             
R1NME2   DS    CL36                                                             
R1NME3   DS    CL36                                                             
R1NME4   DS    CL36                                                             
R1NME5   DS    CL36                                                             
                                                                                
R1COL1   DS    PL8                                                              
R1COL2   DS    PL8                                                              
R1COL3   DS    PL8                                                              
R1COL4   DS    PL8                                                              
R1COL5   DS    PL8                                                              
R1COL6   DS    PL8                                                              
R1COL7   DS    PL8                                                              
R1COL8   DS    PL8                                                              
R1COL9   DS    PL8                                                              
R1COL10  DS    PL8                                                              
R1LEN    EQU   *-R1RECD                                                         
                                                                                
FGROUP   DSECT                                                                  
FGROUP#  DS    CL1                                                              
FGRLN    DS    AL1                                                              
FGRDATA  DS    0CL1                                                             
         EJECT                                                                  
                                                                                
*        ACAPGGEND                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGGEND                                                      
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051APGHFIUBC 02/21/03'                                      
         END                                                                    
