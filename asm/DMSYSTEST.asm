*          DATA SET DMSYSTEST  AT LEVEL 002 AS OF 04/22/15                      
*PHASE SYSTESTA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE DMDMGRL                                                                
         TITLE 'DMSYSTEST - TEST DMSYSTAB/DMFILES TABLES'                       
         PRINT NOGEN                                                            
SYSTEST  CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE 0,SYSTEST,WORK=A(WORK)                                           
*                                                                               
INIT     DS    0H                                                               
         MVC   P(30),=CL30'CONTROL CARDS:'                                      
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P(30),=30C'='                                                    
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
*                                                                               
INIT10   DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    INITX                                                            
* PRINT IT                                                                      
         MVC   P(L'CARD),CARD                                                   
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
*                                                                               
         CLI   CARD,C'*'           COMMENT                                      
         BE    INIT10                                                           
*                                                                               
         CLC   =C'DSPACE=',CARD                                                 
         BNE   INIT20                                                           
*                                                                               
         ICM   RF,15,=V(SSB)       DSPACE=                                      
         JZ    *+2                                                              
         CLI   SSOXTND-SSOOFF(RF),X'FF'                                         
         JNE   *+2                                                              
         MVC   SSODSPAC-SSOOFF(1,RF),CARD+7                                     
         B     INIT10              READ NEXT CARD                               
*                                                                               
INIT20   DS    0H                                                               
         CLC   =C'DDSIO=',CARD                                                  
         JNE   *+2                 NO OTHER CARDS SUPPORTED FOR NOW             
         ICM   RF,15,VDDSIO                                                     
         JZ    *+2                                                              
         MVC   0(8,RF),CARD+6                                                   
         B     INIT10                                                           
*                                                                               
INITX    DS    0H                                                               
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
         B     SYST                                                             
*                                                                               
*                                                                               
*                                                                               
EXIT     SR    RF,RF               SET CONDITION CODE                           
         CP    ERRCNT,=P'0'                                                     
         BE    *+8                                                              
         LHI   RF,8                                                             
         XBASE RC=(RF)                                                          
                                                                                
*                                                                               
*                                                                               
*                                                                               
* PROCESS ALL ENTRIES IN SYSTAB AND CROSS CHECK TO SYSFLES                      
*                                                                               
SYST     DS    0H                                                               
         MVC   P(30),=CL30'SYSTAB CHECK'                                        
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P(30),=30C'='                                                    
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
*                                                                               
         GOTO1 =V(DMSYSFIL),DMCB                                                
         ICM   R2,15,0(R1)         R2=A(SYSTAB ENTRY)                           
         JZ    *+2                                                              
         ST    R2,ASYSTAB                                                       
*                                                                               
SYST1    CLI   0(R2),0             BUMP TO NEXT SYSTAB ENTRY                    
         BE    SYSTX                                                            
         CLI   0(R2),C' '          EMPTY ENTRY                                  
         BNE   SYST2                                                            
*                                                                               
SYST1A   LA    R2,L'SYSTAB(R2)                                                  
         B     SYST1                                                            
*                                                                               
SYST2    MVC   P(8),0(R2)          PRINT SYSTEM NAMES                           
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         XC    DUB,DUB                                                          
         XC    FUL,FUL                                                          
         SR    R3,R3                                                            
         ICM   R3,7,9(R2)          R3=A(FILE NUMBERS INDEX TABLE)               
         ST    R3,DUB                                                           
         BZ    SYSTER1                                                          
         ICM   R4,15,12(R2)                                                     
         ST    R4,DUB+4                                                         
         BZ    SYSTER2             R4=A(SYSFLES ENTRY FOR SYSTEM)               
*                                                                               
SYST3    LHI   R5,256              R5=COUNT OF INDEX TABLE                      
         LR    R6,R3               R6=A(INDEX TABLE ENTRY)                      
SYST3A   CLI   0(R6),0                                                          
         BNE   SYST4                                                            
SYST3X   LA    R6,1(R6)                                                         
         BCT   R5,SYST3A                                                        
         LA    R2,L'SYSTAB(R2)     BUMP TO NEXT SYSTEM                          
         B     SYST1                                                            
*                                                                               
SYST4    LR    R0,R6               FUL+0(1)=FILE NUMBER                         
         SR    R0,R3                                                            
         STC   R0,FUL                                                           
         SR    R7,R7               R7=INDEX NUMBER INTO SYSFLES LIST            
         IC    R7,0(R6)                                                         
         AHI   R7,-1                                                            
         SLL   R7,3                                                             
         LA    R7,4(R7,R4)         R7=A(SYSFLES ENTRY FOR FILE)                 
         MVC   FUL+1(1),3(R7)      FUL+1(1)=FILE NUMBER FROM SYSFLES            
         MVI   P+9,C' '                                                         
         CLC   FUL(1),FUL+1        TEST FILE NUMBERS MATCH                      
         BE    *+8                                                              
         MVI   P+9,C'>'                                                         
         CLI   P+9,C' '            ONLY PRINT ERRORS                            
         BNE   SYSTER3                                                          
         B     SYST3X                                                           
*                                                                               
SYSTER1  MVI   ERRNUM,1                                                         
         MVI   P+9,C'>'                                                         
         MVC   P+10(30),=CL30'MISSING FILE NUM TABLE ADDRESS'                   
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P+9(123),SPACES                                                  
         B     SYST1A                                                           
*                                                                               
SYSTER2  MVI   ERRNUM,2                                                         
         MVI   P+9,C'>'                                                         
         MVC   P+10(30),=CL30'MISSING SYSFLES LIST ADDRESS'                     
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P+9(123),SPACES                                                  
         B     SYST1A                                                           
*                                                                               
SYSTER3  MVI   ERRNUM,3                                                         
         MVI   P+9,C'>'                                                         
         MVC   P+10(30),=CL30'FILE NUMBERS MISMATCH XX XX   '                   
         GOTO1 =V(HEXOUT),PLIST,FUL+0,P+32,1,=C'TOG'                            
         GOTO1 =V(HEXOUT),PLIST,FUL+1,P+35,1,=C'TOG'                            
         LHI   R1,256                                                           
         SR    R1,R5                                                            
         STC   R1,FUL+3                                                         
         GOTO1 =V(HEXOUT),PLIST,FUL+3,P+38,1,=C'TOG'                            
*                                                                               
SYSTERX  GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P+9(123),SPACES                                                  
         AP    ERRCNT,=P'1'                                                     
         CP    ERRCNT,=P'100'                                                   
         BH    EXIT                                                             
         B     SYST3X                                                           
*                                                                               
SYSTX    DS    0H                                                               
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
                                                                                
* PROCESS ALL ENTRIES IN SYSFLES AND CROSS CHECK TO SYSTAB                      
*                                                                               
SYSF     DS    0H                                                               
         MVC   P(30),=CL30'SYSFLES CHECK'                                       
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P(30),=30C'='                                                    
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL02'                                     
*                                                                               
         L     R2,ASYSTAB                                                       
         LA    R2,16(R2)           FIRST NON-BLANK ENTRY IN SYSTAB(SER)         
         ICM   R2,15,12(R2)        A(SYSL01)                                    
*                                                                               
SYSF1    LHI   R3,1                R3=FILE ENTRY NUMBER                         
         LA    R4,4(R2)            R4=A(FILE ENTRY)                             
         MVC   Q,SPACES                                                         
         SR    R8,R8               R8=COUNT OF FILE NAMES LOCATED               
         CLI   0(R2),255                                                        
         BE    EXIT                END OF SYSFLES LIST                          
*                                                                               
SYSF2    MVC   P(3),=C'SE='        PRINT SYSLIST SENUM AND NUM OF FILES         
         GOTO1 =V(HEXOUT),PLIST,0(R2),P+3,1,=C'TOG'                             
         LH    R0,2(R2)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+6(2),DUB+6(2)                                                  
*                                                                               
SYSF3    SR    R5,R5               USE SE NUM TO INDEX INTO SYSTAB              
         IC    R5,0(R2)                                                         
         SLL   R5,4                                                             
         A     R5,ASYSTAB                                                       
*                                                                               
         SR    R6,R6                                                            
         ICM   R6,7,9(R5)          R6=A(SYSTEM FILE NUMBER TABLE)               
         BZ    SYSFER1                                                          
         C     R2,12(R5)           CHECK V(SYSLFES) ADDRESS MATCHES             
         BNE   SYSFER2                                                          
*                                                                               
SYSF4    SR    R7,R7               INDEX INTO FILE NUMBER TABLE                 
         ICM   R7,1,3(R4)                                                       
         AR    R7,R6                                                            
         MVC   FUL+1(1),0(R7)      FULL+1(1)=SYSFLES LIST FILE NUM              
         STC   R3,FUL              FULL+0(1)=SYSFLES LIST ENTRY NUM             
         MVI   P+9,C' '                                                         
         CLC   FUL(1),FUL+1        TEST FILE NUMBERS MATCH                      
         BE    *+8                                                              
         MVI   P+9,C'>'                                                         
         CLI   P+9,C' '            ONLY PRINT ERRORS                            
         BNE   SYSFER3                                                          
*                                                                               
         CHI   R8,13               MOVE FILE NAME FOR PRINTING                  
         BNL   SYSF5                                                            
         SR    RF,RF                                                            
         ICM   RF,7,5(R4)          RF=A(DTF)                                    
         LR    RE,R8                                                            
         MHI   RE,9                                                             
         LA    RE,Q(RE)                                                         
         MVC   0(8,RE),22(RF)                                                   
         AHI   R8,1                BUMP COUNT OF FILE NAMES                     
*                                                                               
SYSF5    AHI   R3,1                BUMP TO NEXT SYSFLES FILE ENTRY              
         LA    R4,8(R4)                                                         
         CH    R3,2(R2)                                                         
         BNH   SYSF4               BACK FOR NEXT FILE                           
         MVC   P+9(13*9),Q                                                      
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         LR    R2,R4                                                            
         B     SYSF1               BACK FOR NEXT SYSFLES SYSTEM ENTRY           
*                                                                               
SYSFER1  MVI   ERRNUM,1                                                         
         MVI   P+9,C'>'                                                         
         MVC   P+10(30),=CL30'MISSING FILE NUM TABLE ADDRESS'                   
         B     SYSFERX                                                          
*                                                                               
SYSFER2  MVI   ERRNUM,2                                                         
         MVI   P+9,C'>'                                                         
         MVC   P+10(30),=CL30'SYSFLES ADDRESS DOES NOT MATCH'                   
         B     SYSFERX                                                          
*                                                                               
SYSFER3  MVI   ERRNUM,3                                                         
         MVI   P+9,C'>'                                                         
         MVC   P+10(30),=CL30'FILE NUMBERS MISMATCH XX XX   '                   
         GOTO1 =V(HEXOUT),PLIST,FUL+0,P+32,1,=C'TOG'                            
         GOTO1 =V(HEXOUT),PLIST,FUL+1,P+35,1,=C'TOG'                            
*                                                                               
SYSFERX  GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P+9(123),SPACES                                                  
         AP    ERRCNT,=P'1'                                                     
         CP    ERRCNT,=P'100'                                                   
         BH    EXIT                                                             
         CLI   ERRNUM,3            BACK FOR NEXT SYSFLES FILE                   
         BE    SYSF5                                                            
         LH    R1,2(R2)            NUM OF FILES*8 + 4                           
         SLL   R1,3                                                             
         LA    R2,4(R1,R2)                                                      
         B     SYSF1               BACK FOR NEXT SYSFLES SYSTEM                 
*                                                                               
         LTORG                                                                  
*                                                                               
DUB      DC    D'0'                                                             
PLIST    DC    6F'0'                                                            
DMCB     DC    6F'0'                                                            
FUL      DC    F'0'                                                             
ASYSTAB  DC    A(0)                                                             
VDDSIO   DC    V(DDSIO)                                                         
*                                                                               
ERRNUM   DC    AL1(0)                                                           
ERRCNT   DC    PL3'0'                                                           
*                                                                               
CARD     DC    CL80' '                                                          
SYSTAB   DS    0XL16               DUMMY, TO RESOLVE L'SYSTAB                   
*                                                                               
*                                                                               
PCC      DC    X'00'                                                            
P        DC    CL132' '                                                         
Q        DC    CL132' '                                                         
SPACES   DC    CL132' '                                                         
*                                                                               
         DS    0D                                                               
         DC    C'*WRKWRK*'                                                      
WORK     DC    1000D'0'                                                         
*                                                                               
SSB      DC    X'0000FF',X'40',4X'00',CL8' ',32X'00',A(0),204X'00'              
UTL      DC    F'0',X'01',XL3'00',XL56'00'                                      
*                                                                               
       ++INCLUDE FASSBOFF                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DMSYSTEST 04/22/15'                                      
         END                                                                    
