*          DATA SET PAVEXPL    AT LEVEL 051 AS OF 05/01/02                      
*PHASE T00A0BC,+0,NOAUTO                                                        
*INCLUDE DELEXP2                                                                
         TITLE 'PAV - RECORD TO WORK RECORD EXPLODE'                            
PAVEXPL  CSECT                                                                  
BASE     DS    940C                                                             
         ORG   BASE                                                             
         PRINT NOGEN                                                            
         NMOD1 10,PAVEXPL,RR=R8                                                 
         ST    R8,RELO                                                          
         B     ST                                                               
RELO     DC    A(0)                                                             
ST       L     R5,0(R1)            A(SOURCE/BOOK)                               
         USING PAVEXPW,RC                                                       
         L     R6,8(R1)            A(WORK RECORD)                               
         L     R4,4(R1)            A(FIRST RECORD ELEMENT)                      
         MVC   HLDSRC,0(R5)                                                     
         CLI   HLDSRC,C'P'                                                      
         BE    PROG2                                                            
         LA    R5,0(R5)                                                         
         CLI   HLDSRC,C'A'                                                      
         BNE   *+8                                                              
         MVI   HLDSRC,C'E'                                                      
         CLI   HLDSRC,C'N'                                                      
         BNE   *+8                                                              
         MVI   HLDSRC,C'F'                                                      
         LA    R5,HLDSRC                                                        
         LR    RE,R6                                                            
         LA    RF,480                                                           
         XCEF                                                                   
         USING PWREC,R6                                                         
         ST    R4,PWPNAME                                                       
         MVI   PWNUMFLD,X'6C'                                                   
         LR    RF,R4                                                            
         MVI   PREVLEN,0                                                        
         CLI   HLDSRC,C'Y'                                                      
         BE    PVEXPY                                                           
         L     R2,=V(PWDISP1)                                                   
         A     R2,RELO                                                          
         A     R2,=F'520'                                                       
         XC    0(2,R2),0(R2)                                                    
         B     PVEXP1                                                           
PVEXPY   MVI   HLDSRC,C'E'                                                      
PVEXP1   CLI   0(RF),X'30'                                                      
         BH    PVEXP2                                                           
         CLI   0(RF),X'25'         SOURCE BOOK ELEMENT PRESENT                  
         BNE   *+10                                                             
         MVC   HLDSRC,2(RF)         YES - SET SOURCE/BOOK                       
         CLI   0(RF),0                                                          
         BE    EXIT                                                             
         ZIC   RE,1(RF)                                                         
         MVC   PREVLEN,1(RF)                                                    
         AR    RF,RE                                                            
         B     PVEXP1                                                           
PVEXP2   ZIC   RE,PREVLEN                                                       
         SR    RF,RE                                                            
         LR    R4,RF                                                            
         L     R2,=V(PWDISP1)                                                   
         A     R2,RELO                                                          
         L     RF,=V(DELEXP)                                                    
         A     RF,RELO                                                          
         SR    R3,R3                                                            
         SR    R7,R7                                                            
         STM   R2,R7,PARAM                                                      
         MVI   PARAM+20,C'P'                                                    
         MVI   PARAM,X'02'                                                      
         LA    R1,PARAM                                                         
         GOTO1 (RF)                                                             
         CLC   PWUHOME,=F'763000'       NTI FIX                                 
         BE    NTI80                                                            
         CLC   PWUHOME,=F'778000'                                               
         BE    NTI81                                                            
         CLC   PWUHOME,=F'815000'                                               
         BE    NTI82                                                            
         CLC   PWUWWRK,=F'330900'                                               
         BE    NTI84                                                            
         CLC   PWUHOME,=F'833000'                                               
         BE    NTI83                                                            
         CLC   PWUW1824,PWUHOME    CORRECT NSI NEW YORK UNIVERSE                
         BL    NSIFIX2                                                          
         LA    R7,PWUK611                                                       
         LA    R8,16                                                            
NSIFIX   L     RF,0(R7)                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         ST    RF,0(R7)                                                         
         LA    R7,16(R7)                                                        
         BCT   R8,NSIFIX                                                        
NSIFIX2  CLC   PWTW1824,PWUW1824                                                
         BL    FIXNEG                                                           
         LA    R7,PWTK611                                                       
         LA    R8,16                                                            
NSIFIX3  L     RF,0(R7)                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         ST    RF,0(R7)                                                         
         LA    R7,16(R7)                                                        
         BCT   R8,NSIFIX3                                                       
FIXNEG   LA    R7,PWIK611                                                       
         LA    R8,16                                                            
FIXNEG1  TM    0(R7),X'80'                                                      
         BO    FIXNEG2                                                          
         CLC   0(4,R7),=F'150000'                                               
         BL    *+10                                                             
         XC    0(4,R7),0(R7)                                                    
FIXNEG2  LA    R7,16(R7)                                                        
         BCT   R8,FIXNEG1                                                       
         B     EXIT                                                             
         SPACE 2                                                                
* FIX UP DEMO CONVERSION BUG ON W1824 / W50+ UNIVERSES                          
NTI80    MVC   PWUW1824,=F'140000'                                              
         MVC   PWUWO50,=F'302500'                                               
         B     EXIT                                                             
NTI81    MVC   PWUW1824,=F'140900'                                              
         MVC   PWUWO50,=F'306800'                                               
         B     EXIT                                                             
NTI82    MVC   PWUW1824,=F'145000'                                              
         MVC   PWUW2534,=F'190200'                                              
         MVC   PWUWO50,=F'315700'                                               
         B     EXIT                                                             
NTI83    MVC   PWUW1824,=F'143700'                                              
         MVC   PWUW2534,=F'195400'                                              
         MVC   PWUWO50,=F'322100'                                               
         B     EXIT                                                             
NTI84    MVC   PWUHOME,=F'838000'                                               
         MVC   PWUW1824,=F'140400'                                              
         MVC   PWUW2534,=F'199500'                                              
         MVC   PWUW3549,=F'209800'                                              
         MVC   PWUWO50,=F'325100'                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*              ROUTINE TO EXPLODE FROM NETWORK PROGRAM                          
         SPACE 3                                                                
PROG2    CLI   0(R4),X'92'         POSITION TO ELEMENT                          
         BE    PROG4                                                            
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         CLI   0(R4),0                                                          
         BNE   PROG2                                                            
         DC    H'0'                                                             
         SPACE 2                                                                
         USING NPGELEM,R4                                                       
         USING PWREC,R6                                                         
PROG4    XC    0(16,R6),0(R6)                                                   
         XC    PWPNAME,PWPNAME                                                  
         XC    PWRPO2(76),PWRPO2                                                
         SPACE 1                                                                
         L     R1,PWUHOME          HOMES UNIVERSE                               
         TM    NPGSTAT,X'80'                                                    
         BO    PROG4B                                                           
         MVI   PWNUMFLD,108                                                     
         ZIC   R0,HLDSRC+1                                                      
         SLL   R0,8                                                             
         IC    R0,HLDSRC+2                                                      
         MR    R0,R0               X HUT                                        
         D     R0,=F'500'                                                       
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,PWTHOME          = HOMES WATCHING TV                          
         SPACE 1                                                                
PROG4B   LH    R0,NPGSHARE                                                      
         CH    R0,=H'10'           IGNORE RIDICULOUS SHARES                     
         BH    *+6                                                              
         SR    R0,R0                                                            
         ZIC   R1,HLDSRC+1                                                      
         SLL   R1,8                                                             
         IC    R1,HLDSRC+2                                                      
         MR    R0,R0               SHARE X HUT                                  
         D     R0,=F'500'                                                       
         AH    R1,=H'1'                                                         
         SRA   R1,1                = RATING TO 1 DEC PLACE                      
         TM    NPGSTAT,X'80'       RATING MAY HAVE BEEN INPUT                   
         BNO   *+8                                                              
         LH    R1,NPGSHARE                                                      
         M     R0,PWUHOME          X HOME UNIVERSE                              
         D     R0,=F'500'                                                       
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,PWIHOME          = HOMES IMPRESSIONS                          
         ST    R1,PWMHOME                                                       
         EJECT                                                                  
*              LOOP THROUGH WORK RECORD                                         
         SPACE 3                                                                
         LA    R2,PWDATA                                                        
         LA    R3,VPHTAB                                                        
         BAS   RE,FIXVPH                                                        
         SPACE 2                                                                
PROG5    CLI   0(R3),X'FF'                                                      
         BE    EXIT                                                             
         SR    R0,R0               ADD UP VPHS IN R0                            
         ZIC   R1,0(R3)                                                         
         LTR   R1,R1                                                            
         BZ    PROG6                                                            
         LA    R1,NPGVPHS-1(R1)                                                 
         IC    R0,0(R1)            ADD FIRST ENTRY                              
         SPACE 2                                                                
PROG6    ZIC   R1,1(R3)                                                         
         LTR   R1,R1                                                            
         BZ    PROG8                                                            
         LA    R1,NPGVPHS-1(R1)                                                 
         ZIC   RF,0(R1)                                                         
         AR    R0,RF               TO SECOND (IF PRESENT)                       
         SPACE 2                                                                
PROG8    ZIC   R1,2(R3)                                                         
         LTR   R1,R1                                                            
         BZ    PROG10                                                           
         LA    R1,NPGVPHS-1(R1)                                                 
         ZIC   RF,0(R1)                                                         
         SR    R0,RF               AND SUBTRACT THIRD                           
         SPACE 2                                                                
PROG10   XC    0(12,R2),0(R2)                                                   
         L     R1,PWIHOME          HOMES IMPRESSIONS                            
         MR    R0,R0               X VPH                                        
         D     R0,=F'50'                                                        
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,0(R2)            = PEOPLE IMPRESSIONS                         
         ST    R1,4(R2)                                                         
         SPACE 2                                                                
PROG12   LA    R2,16(R2)                                                        
         LA    R3,3(R3)                                                         
         B     PROG5                                                            
         EJECT                                                                  
*              FORMULA TABLE FOR VPH DEDUCTION                                  
         SPACE 3                                                                
VPHTAB   DS    0H                                                               
         DC    AL1(26,0,0)    CH6-11                                            
         DC    AL1(27,0,0)    CH2-11                                            
         DC    AL1(22,0,0)    WM12-17                                           
         DC    AL1(24,0,0)    VM12-17                                           
         DC    AL1(0,0,0)     WM15-17                                           
         DC    AL1(0,0,0)     WN15-17                                           
         DC    AL1(7,0,10)    WM18-24   WM18-49 - WM25-49                       
         DC    AL1(8,0,11)    WN18-24   WN18-49 - WN25-49                       
         DC    AL1(4,10,7)    WM25-34   WM18-34 + WM25-49 - WM18-49             
         DC    AL1(5,11,8)    MN25-34   MN18-34 + MN25-49 - MN18-49             
         DC    AL1(7,0,4)     WM35-49   WM18-49 - WM18-34                       
         DC    AL1(8,0,5)     WN35-49   WN18-49 - WN18-34                       
         DC    AL1(13,0,10)   WM50-54   WM25-54 - WM25-49                       
         DC    AL1(14,0,11)   WN50-54   WN25-54 - MN25-49                       
         DC    AL1(16,13,10)  WN50-64   WM55-64 + WN25-54 - WM25-49             
         DC    AL1(17,14,11)  WN50-64   WN55-64 + MN55-64 - MN25-49             
         DC    AL1(1,0,7)     WM50+     WOMEN   - WM18-49                       
         DC    AL1(2,0,8)     MN50+     MEN     - MN18-49                       
         DC    AL1(0,0,0)     METRO                                             
         DC    AL1(29,0,0)    WWRK                                              
         DC    AL1(28,0,0)    LOH                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
FIXVPH   NTR1                                                                   
*                                  ROUTINE TO DEDUCE MISSING VPHS               
         LA    R2,NPGVPHS                                                       
         LA    R3,9                                                             
         SPACE 2                                                                
FIXVPH2  ZIC   R4,0(R2)                                                         
         ZIC   R5,1(R2)                                                         
         ZIC   R6,2(R2)                                                         
         LTR   R4,R4                                                            
         BNZ   FIXVPH4                                                          
         LR    R4,R6                                                            
         SR    R4,R5                                                            
         STC   R4,0(R2)                                                         
         SPACE 1                                                                
FIXVPH4  LTR   R5,R5                                                            
         BNZ   FIXVPH6                                                          
         LR    R5,R6                                                            
         SR    R5,R4                                                            
         STC   R5,1(R2)                                                         
         SPACE 1                                                                
FIXVPH6  AR    R4,R5                                                            
         LTR   R6,R6                                                            
         BNZ   *+8                                                              
         STC   R4,2(R2)                                                         
         LA    R2,3(R2)                                                         
         BCT   R3,FIXVPH2                                                       
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
PAVEXPW  DSECT                                                                  
HLDSRC   DS    CL3                                                              
PREVLEN  DS    CL1                                                              
PARAM    DS    10F                                                              
         EJECT                                                                  
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE DMPAVWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DEPAVDISP                                                      
