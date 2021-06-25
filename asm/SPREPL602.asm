*          DATA SET SPREPL602  AT LEVEL 020 AS OF 05/01/02                      
*PHASE SPL602A                                                                  
         TITLE 'SPL602 - SPOTPAK COMMENT LISTING'                               
SPL602   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPL602,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         STM   R9,RB,SPL6R9                                                     
         SPACE 2                                                                
         CLI   MODE,PROCREC                                                     
         BE    PROCESS                                                          
         CLI   MODE,REQFRST                                                     
         BE    HEADLINE                                                         
         CLI   MODE,CLTFRST                                                     
         BNE   HD01                                                             
         CLI   RCSUBPRG,2                                                       
         BL    EXIT                                                             
         B     HEADLINE                                                         
HD01     CLI   MODE,PRDFRST                                                     
         BNE   HD02                                                             
         CLI   RCSUBPRG,3                                                       
         BL    EXIT                                                             
         B     HEADLINE                                                         
HD02     CLI   MODE,ESTFRST                                                     
         BNE   EXIT                                                             
         CLI   RCSUBPRG,4                                                       
         BL    EXIT                                                             
*                                                                               
HEADLINE L     RE,=A(SPL6HL)                                                    
         A     RE,RELO                                                          
         ST    RE,HEADHOOK                                                      
         MVC   P,SPACES                                                         
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
PROCESS  DS    0H                                                               
         XC    SVLN,SVLN                                                        
         LA    R6,SVLN                                                          
         CLI   RCSUBPRG,1                                                       
         BH    CP300                                                            
         CLC   CLT,SPACES                                                       
          BE    PROCESS2                                                        
         MVC   0(6,R6),=C'CLIENT'                                               
         MVC   9(3,R6),CLT                                                      
         MVC   13(15,R6),CLTNM                                                  
         LA    R6,34(R6)                                                        
*                                                                               
CP300    CLI   RCSUBPRG,2                                                       
         BH    CP310                                                            
         CLC   PGR1,SPACES                                                      
         BE    CP301                                                            
         MVC   0(13,R6),=C'PRODUCT GROUP'                                       
         MVC   16(5,R6),PGR1                                                    
         MVC   22(15,R6),PGR1NM                                                 
         LA    R6,37(R6)                                                        
         B     CP310                                                            
CP301    CLC   PRD,SPACES                                                       
         BE    CP310                                                            
         MVC   0(7,R6),=C'PRODUCT'                                              
         MVC   10(3,R6),PRD                                                     
         MVC   14(15,R6),PRDNM                                                  
         LA    R6,34(R6)                                                        
*                                                                               
CP310    CLI   RCSUBPRG,3                                                       
         BH    PROCESS2                                                         
         CLC   EST,SPACES                                                       
         BE    PROCESS2                                                         
         MVC   0(8,R6),=C'ESTIMATE'                                             
         MVC   11(3,R6),EST                                                     
         MVC   16(15,R6),ESTNM                                                  
         LA    R6,36(R6)                                                        
         CLI   QOPT1,C'M'          TEST MEDIA COMMENT                           
         BNE   PROCESS2                                                         
         MVC   0(6,R6),=C'MARKET'  YES-PRINT THE MARKET                         
         MVC   9(4,R6),MKT                                                      
         MVC   15(24,R6),MKTNM                                                  
         EJECT                                                                  
PROCESS2 DS    0H                                                               
         USING COMHDRD,R6                                                       
         LA    R2,PCOMMNT                                                       
         XC    0(132,R2),0(R2)                                                  
         L     R6,AREC                                                          
         TM    COMPROF1,X'80'      OPTIONS                                      
         BZ    PT0A                                                             
         MVC   23(8,R2),=C'PAGE=ALL'                                            
* IN THE FUTURE, PLACE OPTIONS ALONG THE LINE AND USE SQUASHER                  
*                                                                               
PT0A     GOTO1 DATCON,DMCB,(3,COMACTIV),(5,DUB)                                 
         MVC   4(8,R2),DUB                                                      
*                                                                               
         LA    R5,2                                                             
         LA    R2,50(R2)                                                        
         LA    R6,24(R6)                                                        
         DROP  R6                                                               
         BAS   RE,NEXTEL2                                                       
*****    BNE   PT3                                                              
         BNE   EXIT                NO COMMENTS - IGNORE                         
         LA    R5,1(R5)            FIRST LINE                                   
         CLI   0(R6),X'05'                                                      
         BNE   PT2                                                              
         LR    R7,R6                                                            
*                                                                               
PT0B     ZIC   R3,1(R7)                                                         
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R7,R3                                                            
         CLI   0(R7),X'00'                                                      
         BE    PT1B                                                             
         CLI   0(R7),X'15'         ANY PRINT=BOTTOM                             
         BNE   PT0B                                                             
         XC    0(132,R2),0(R2)                                                  
         MVC   0(9,R2),=C'PRINT=TOP'                                            
*                                                                               
PT1A     LA    R2,132(R2)          ADVANCE TO NEXT LINE                         
         XC    0(132,R2),0(R2)                                                  
         LA    R5,1(R5)            ADD TO LINE COUNT                            
*                                                                               
PT1B     ZIC   R3,1(R6)                                                         
         LA    R4,3                                                             
         SR    R3,R4               FIGURE LENGTH OF COMMENT                     
         EX    R3,LINEMOVE                                                      
         BAS   RE,NEXTEL                                                        
         BNE   PT3                                                              
         CLI   0(R6),X'05'         MORE PRINT TOP                               
         BE    PT1A                                                             
         LA    R2,132(R2)                                                       
         LA    R5,1(R5)                                                         
*                                                                               
PT2      XC    0(132,R2),0(R2)                                                  
         MVC   0(12,R2),=C'PRINT=BOTTOM'                                        
*                                                                               
PT2A     LA    R2,132(R2)          NEXT LINE                                    
         XC    0(132,R2),0(R2)                                                  
         LA    R5,1(R5)                                                         
         ZIC   R3,1(R6)                                                         
         LA    R4,3                                                             
         SR    R3,R4                                                            
         EX    R3,LINEMOVE                                                      
         BAS   RE,NEXTEL                                                        
         BNE   PT3                                                              
         CLI   0(R6),X'15'                                                      
         BE    PT2A                                                             
*                                                                               
PT3      STC   R5,ALLOWLIN         NUMBER OF LINES NEEDED                       
         CLI   ALLOWLIN,2                                                       
         BH    *+6                                                              
         DC    H'0'                NO LINES TRAP                                
         MVC   P1,SVLN             HEADING FOR COMMENT                          
         MVI   P2,X'00'                                                         
         LA    R6,P3                                                            
         LA    R2,PCOMMNT                                                       
         LA    R7,12               MAX NO. OF COMMENT LINES THIS TRIP           
         CLI   ALLOWLIN,14                                                      
         BH    PT4                 MOVE ALL THIS TRIP LINES TO PLACE            
         BCTR  R5,0                ONE TRIP - ADJUST NO. OF LINES               
         BCTR  R5,0                                                             
         LR    R7,R5                                                            
*                                                                               
PT4      MVC   0(132,R6),0(R2)                                                  
         LA    R2,132(R2)                                                       
         LA    R6,132(R6)                                                       
         BCT   R7,PT4                                                           
         CLI   ALLOWLIN,14                                                      
         BNH   REPORT2                                                          
         ZIC   R7,ALLOWLIN                                                      
         SH    R7,=H'14'                                                        
         STC   R7,SVR7                                                          
         GOTO1 REPORT              SEND PART ONE                                
         LA    R6,P1                                                            
         LA    R2,PCMMNT13                                                      
         ZIC   R7,SVR7                                                          
PT4A     MVC   0(132,R6),0(R2)                                                  
         LA    R2,132(R2)                                                       
         LA    R6,132(R6)                                                       
         BCT   R7,PT4A                                                          
REPORT2  DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT              LAST TRIP                                    
         B     EXIT                                                             
         SPACE 2                                                                
LINEMOVE MVC   0(0,R2),2(R6)       EXECUTED                                     
         EJECT                                                                  
NEXTEL   CLI   0(R6),0             END                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                 ZERO LENGTH TRAP                             
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),X'05'         FIND 05 OR 15 ELEMENTS                       
         BER   RE                                                               
         CLI   0(R6),X'15'                                                      
         BER   RE                                                               
         B     NEXTEL                                                           
NEXTELX  LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
* HEADLINE HOOK ROUTINE                                                         
*                                                                               
         USING *,RF                                                             
SPL6HL   NTR1  BASE=SPL6RB                                                      
         DROP RF                                                                
         LM    R9,RA,SPL6R9                                                     
         USING SPWORKD,RA,R9                                                    
         LA    R6,CMTABLE                                                       
*                                                                               
CPCMTYPE CLI   0(R6),X'FF'                                                      
         BE    CPMATCH                                                          
         CLC   0(1,R6),KEY+3                                                    
         BE    CPMATCH                                                          
         LA    R6,16(R6)                                                        
         B     CPCMTYPE                                                         
*                                                                               
CPMATCH  MVC   SVH4(132),H4                                                     
         MVC   SVH4+50(14),=C'COMMENT TYPE -'                                   
         MVC   SVH4+65(15),1(R6)                                                
         MVC   H4,SVH4                                                          
*                                                                               
         CLI   RCSUBPRG,1          MEDIA PAGING                                 
         BNH   EXIT                                                             
         CLC   CLT,SPACES          CLIENT PAGING                                
         BE    EXIT                                                             
         MVC   SVH4+1(6),=C'CLIENT'                                             
         MVC   SVH4+10(3),CLT                                                   
         MVC   SVH4+14(24),CLTNM                                                
         MVC   H4,SVH4                                                          
*                                                                               
         CLI   RCSUBPRG,2                                                       
         BNH   EXIT                                                             
         MVC   SVH5,H5                                                          
         CLC   PRD,SPACES          PRODUCT PAGING                               
         BNE   CP400                                                            
         CLC   PGR1,SPACES                                                      
         BE    EXIT                                                             
         MVC   SVH5+1(13),=C'PRODUCT GROUP'                                     
         MVC   SVH5+15(5),PGR1                                                  
         MVC   SVH5+21(24),PGR1NM                                               
         B     CP410                                                            
CP400    MVC   SVH5+1(7),=C'PRODUCT'                                            
         MVC   SVH5+10(3),PRD                                                   
         MVC   SVH5+14(24),PRDNM                                                
*                                                                               
CP410    MVC   H5,SVH5                                                          
         CLI   RCSUBPRG,3                                                       
         BNH   EXIT                                                             
         MVC   SVH6,H6                                                          
         CLC   EST,SPACES          ESTIMATE PAGING                              
         BE    EXIT                                                             
         MVC   SVH6+1(8),=C'ESTIMATE'                                           
         MVC   SVH6+10(3),EST                                                   
         MVC   SVH6+14(24),ESTNM                                                
         MVC   H6,SVH6                                                          
         B     EXIT                                                             
*                                                                               
SPL6R9   DC    F'0'                                                             
SPL6RA   DC    F'0'                                                             
SPL6RB   DC    F'0'                                                             
SVR7     DC    X'00'                                                            
*                                                                               
SVLN     DC    CL132' '                                                         
SVH4     DC    CL132' '                                                         
SVH5     DC    CL132' '                                                         
SVH6     DC    CL132' '                                                         
         SPACE 2                                                                
CMTABLE  DC    C'BBILLING        '                                              
         DC    C'IINVOICE MATCH  '                                              
         DC    C'MMEDIA SUMMARY  '                                              
         DC    C'PPRODUCT SUMMARY'                                              
         DC    C'APRODUCT SUMMARY'                                              
         DC    C'*               '                                              
         DC    X'FF',C'               '                                         
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
PCOMMNT  DS    16CL132                                                          
         ORG   PCOMMNT                                                          
PCMMNT1  DC    CL132' '                                                         
PCMMNT2  DC    CL132' '                                                         
PCMMNT3  DC    CL132' '                                                         
PCMMNT4  DC    CL132' '                                                         
PCMMNT5  DC    CL132' '                                                         
PCMMNT6  DC    CL132' '                                                         
PCMMNT7  DC    CL132' '                                                         
PCMMNT8  DC    CL132' '                                                         
PCMMNT9  DC    CL132' '                                                         
PCMMNT10 DC    CL132' '                                                         
PCMMNT11 DC    CL132' '                                                         
PCMMNT12 DC    CL132' '                                                         
PCMMNT13 DC    CL132' '                                                         
PCMMNT14 DC    CL132' '                                                         
PCMMNT15 DC    CL132' '                                                         
PCMMNT16 DC    CL132' '                                                         
         EJECT                                                                  
COMHDRD  DSECT                                                                  
       ++INCLUDE SPGENCOM                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SPREPL602 05/01/02'                                      
         END                                                                    
