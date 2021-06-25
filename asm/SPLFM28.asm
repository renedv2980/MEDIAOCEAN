*          DATA SET SPLFM28    AT LEVEL 020 AS OF 05/01/02                      
*PHASE T21928A,+0                                                               
*INCLUDE DPTRD                                                                  
         TITLE 'SPLFM28 - DAYPART SPLIT RECORD'                                 
T21928   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21928                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING SPLTRECD,R8                                                      
         CLI   SVFMTSW,0           TEST FORMAT OR EDIT                          
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
FMT      DS    0H                                                               
*                                  READ DAYPART MENU INTO REC2                  
         MVC   DMCB(2),SVAALPHA    AGENCY                                       
         MVC   DMCB+2(1),SVEBCMED  MEDIA                                        
         MVC   DMCB+3(1),SVAEDATS  MENU                                         
         GOTO1 =V(DPTRD),DMCB,,REC2,VDATAMGR,RR=RB                              
         CLI   8(R1),X'FF'                                                      
         BE    FMTERR                                                           
         LA    R4,SLTDPT1H                                                      
         LA    R5,35               FOR BCT (MAX ON SCREEN)                      
         LA    R6,REC2                                                          
*                                                                               
FMT10    CLI   0(R6),0                                                          
         BE    FMT15                                                            
         CLI   0(R6),C'Z'          SKIP DAYPART Z                               
         BE    FMT15                                                            
         MVC   8(1,R4),0(R6)                                                    
         MVC   10(3,R4),2(R6)                                                   
         B     FMT20                                                            
*                                                                               
FMT15    XC    8(5,R4),8(R4)                                                    
FMT20    FOUT  (R4)                                                             
         LA    R4,LEN1(R4)         NEXT PROTECTED HEADER                        
         CLI   0(R6),0                                                          
         BE    *+8                 END OF LIST  - DON'T KEEP BUMPING            
         LA    R6,5(R6)                                                         
         BCT   R5,FMT10                                                         
*                                                                               
FMT25    DS    0H                                                               
         OC    SVKEY+14(4),SVKEY+14          SEE IF I HAVE A RECORD             
         BZ    FMT90               NO                                           
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         LA    R7,REC+24                                                        
         MVI   ELCODE,X'02'                                                     
         CLI   0(R7),X'02'                                                      
         BE    FMT35                                                            
FMT30    BAS   RE,NEXTEL                                                        
         BNE   FMT70               NO 02 ELEM                                   
FMT35    DS    0H                                                               
         USING SPLTEL02,R7                                                      
         LA    R2,SLTFLD1H                                                      
         LA    R3,35                                                            
*                                                                               
FMT40    XC    8(5,R2),8(R2)                                                    
         FOUT  (R2)                                                             
         LA    R2,LEN2(R2)                                                      
         BCT   R3,FMT40                                                         
*                                                                               
         LA    R2,SPLTDAYP                                                      
*                                                                               
FMT45    LA    R4,SLTDPT1H                                                      
         LA    R5,SLTFLD1H                                                      
         LA    R6,35                                                            
         CLI   0(R2),0                                                          
         BE    FMT70                                                            
FMT50    CLC   8(1,R4),0(R2)                                                    
         BNE   FMT55                                                            
         OC    1(2,R2),1(R2)       DON'T PRINT .00                              
         BZ    FMT52                                                            
         EDIT  (B2,1(R2)),(5,8(R5)),2                                           
FMT52    LA    R2,3(R2)                                                         
         B     FMT45                                                            
*                                                                               
FMT55    DS    0H                                                               
         LA    R4,LEN1(R4)                                                      
         LA    R5,LEN2(R5)                                                      
         BCT   R6,FMT50                                                         
         LA    R2,3(R2)            CAN'T FIND DAYPART - BUMP TO NEXT            
         B     FMT45                                                            
*                                                                               
FMT70    DS    0H                                                               
FMT90    DS    0H                                                               
         XC    SLTMSG,SLTMSG                                                    
*                                                                               
         LA    R7,REC+24                                                        
         CLI   0(R7),X'01'                                                      
         BE    FMT95                                                            
         MVI   ELCODE,X'01'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   FMT95X                                                           
*                                                                               
FMT95    MVC   SLTMSG(13),=C'LAST ACTIVITY'                                     
         USING SPLTEL01,R7                                                      
         GOTO1 VDATCON,DMCB,(3,SPLTACTD),(5,SLTMSG+14)                          
         MVC   SLTMSG+24(3),=C'ADD'                                             
         CLI   SPLTACT,C'A'                                                     
         BE    *+10                                                             
         MVC   SLTMSG+24(6),=C'CHANGE'                                          
*                                                                               
FMT95X   FOUT  SLTMSGH                                                          
         B     EXXMOD                                                           
*                                                                               
FMTERR   LA    R2,LFMKEYH          CURSOR TO KEY                                
         MVI   ERRAREA,X'01'                                                    
         MVC   LFMMSG(39),=C'** ESTIMATE''S DAYPART MENU NOT FOUND **'          
         FOUT  LFMMSGH                                                          
         B     EXXMOD                                                           
         EJECT                                                                  
EDT      DS    0H                                                               
         CLI   SVACT,C'A'          SEE IF ADD                                   
         BE    EDT2                                                             
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC              REREAD REC                                   
*                                                                               
*                                                                               
EDT2     XC    ELEM(100),ELEM                                                   
         MVC   ELEM(2),=X'0232'                                                 
         XC    FULL,FULL                                                        
         LA    R6,ELEM                                                          
         USING SPLTEL02,R6                                                      
         LA    R3,SLTDPT1H                                                      
         LA    R2,SLTFLD1H                                                      
         LA    R7,SPLTDAYP                                                      
         LA    R5,35               MAX DAYPARTS ON SCREEN                       
*                                                                               
EDT8     CLI   5(R2),0             TEST ANY PCT INPUT                           
         BE    EDT20               NO                                           
         CLI   8(R3),C' '                                                       
         BNH   EDT20                                                            
         ZIC   R4,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,8(R2),(R4)                                         
         CLI   DMCB,X'FF'                                                       
         BE    EDTERR                                                           
         L     R0,DMCB+4                                                        
         C     R0,=F'0'                                                         
         BL    EDTERR              NEGATIVE VALUE - INVALID                     
         BE    EDT20               ZERO VALUE - IGNORE                          
         C     R0,=F'10000'        CAN'T EXCEED 100.00                          
         BH    EDTERR                                                           
*                                                                               
         MVC   0(1,R7),8(R3)       MOVE DPT CODE                                
         L     R1,FULL                                                          
         AR    R1,R0                                                            
         ST    R1,FULL                                                          
         STH   R0,HALF                                                          
         MVC   1(2,R7),HALF                                                     
*                                                                               
         LA    R7,3(R7)                                                         
         LA    R0,SPLTDAYP+L'SPLTDAYP-3                                         
         CR    R0,R7               TEST TOO MANY SPLIT PERCENTAGES              
         BNL   EDT20               NO                                           
*                                                                               
         LA    R2,LFMKEYH          CURSOR TO KEY                                
         MVI   ERRAREA,X'01'                                                    
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(41),=C'** NO MORE THAN 15 PERCENTAGES ALLOWED **'         
         FOUT  LFMMSGH                                                          
         B     EXXMOD                                                           
*                                                                               
EDT20    LA    R3,LEN1(R3)                                                      
         LA    R2,LEN2(R2)                                                      
         BCT   R5,EDT8                                                          
*                                                                               
EDT30    DS    0H                                                               
         CLC   FULL,=F'10000'      MUST TOTAL 100 PCT                           
         BE    EDT33                                                            
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(44),=C'** TOTAL OF PCTS MUST BE 100.00 IS NOW 00.X        
               00'                                                              
         LA    R5,LFMMSG+39                                                     
         EDIT  (B4,FULL),(6,0(R5)),2                                            
         FOUT  LFMMSGH                                                          
         LA    R2,SLTFLD1H         CURSOR TO FIRST FIELD                        
         OI    6(R2),X'40'                                                      
         MVI   ERRAREA,X'01'                                                    
         B     EXXMOD                                                           
*                                                                               
EDT33    DS    0H                                                               
         CLI   SVACT,C'A'          SEE IF ADD                                   
         BNE   EDT35                                                            
         MVC   REC+24(50),ELEM                                                  
         MVC   SPLTLEN,=H'74'      50+24                                        
         B     WRITE                                                            
*                                                                               
EDT35    DS    0H                                                               
         LA    R7,REC+24                                                        
         MVI   ELCODE,X'02'                                                     
         CLI   0(R7),X'02'                                                      
         BE    EDT40                                                            
         BAS   RE,NEXTEL                                                        
         BE    EDT40                                                            
         DC    H'0'                                                             
*                                                                               
EDT40    DS    0H                                                               
         GOTO1 VRECUP,DMCB,(0,REC),(R7),0      DELETE OLD 92                    
*                                                                               
         GOTO1 VRECUP,DMCB,(0,REC),ELEM,(R7)       ADD NEW ONE                  
*                                                                               
WRITE    DS    0H                                                               
         BAS   RE,ACTIVITY         ADD OR UPDATE ACTIVITY ELEM                  
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         MVC   KEY,SVKEY                                                        
         MVC   REC(13),SVKEY                                                    
         CLI   SVACT,C'A'          SEE IF ADD                                   
         BE    ADDNPGM                                                          
         LA    R0,REC2                                                          
         ST    R0,AREC                                                          
         GOTO1 GETREC              REREAD REC                                   
         ST    R8,AREC                                                          
         GOTO1 PUTREC                                                           
         B     FMT                                                              
*                                                                               
ADDNPGM  GOTO1 ADDREC                                                           
         MVC   SVKEY,KEY                                                        
         B     FMT                 GO REFORMAT REC                              
         EJECT                                                                  
ACTIVITY NTR1                                                                   
         XC    ELEM+200(10),ELEM+200                                            
         MVC   ELEM+200(2),=X'0108'                                             
         LA    R7,ELEM+200                                                      
         USING SPLTEL01,R7                                                      
         GOTO1 VDATCON,DMCB,(5,0),(3,SPLTACTD)                                  
*                                  SAVE ACTIVITY DATE - TODAY                   
         MVC   SPLTACT,SVACT                                                    
         LA    R7,REC+24                                                        
         CLI   0(R7),X'01'                                                      
         BE    ACT2                                                             
         MVI   ELCODE,X'01'                                                     
         BAS   RE,NEXTEL                                                        
         BE    ACT2                                                             
         LA    R7,REC+24           ADD AS FIRST ELEM+200                        
         GOTO1 VRECUP,DMCB,(0,REC),ELEM+200,(R7)                                
         B     ACTX                                                             
*                                                                               
ACT2     MVC   0(8,R7),ELEM+200    SWITCH OLD AND NEW ELEMS                     
*                                                                               
ACTX     XIT1                                                                   
         EJECT                                                                  
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R7)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  LTR   R7,R7                                                            
         BR    RE                                                               
*                                                                               
EDTERR   MVI   ERRCD,INVERR                                                     
*                                                                               
LFMERR   GOTO1 ERROR                                                            
         LTORG                                                                  
       ++INCLUDE SPLFMWRK                                                       
         ORG   LFMTABH                                                          
*SPLFME8                                                                        
       ++INCLUDE SPLFME8D                                                       
*                                                                               
LEN1     EQU   SLTDPT2H-SLTDPT1H                                                
LEN2     EQU   SLTFLD2H-SLTFLD1H                                                
         EJECT                                                                  
       ++INCLUDE SPGENSPLT                                                      
         EJECT                                                                  
       ++INCLUDE SPGENDAYPT                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SPLFM28   05/01/02'                                      
         END                                                                    
