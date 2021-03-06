*          DATA SET DDDAYVAL   AT LEVEL 011 AS OF 08/13/00                      
*PHASE T00A03A                                                                  
         TITLE 'MODULE TO VALIDATE DAY EXPRESSIONS'                             
DAYVAL   CSECT                                                                  
         NMOD1 15,**DAVL**                                                      
         USING DAYD,RC                                                          
         L     R8,8(R1)            PARA3 A(1 BYTE ST-END OUTPUT)                
         LM    R2,R3,0(R1)         PARA1 BYTE 0   L'INPUT                       
         SR    R4,R4                          1-3 A(INPUT)                      
         IC    R4,0(R1)            PARA2          A(1 BYTE OUTPUT)              
         LTR   R4,R4                              OUTPUT SET TO ZERO            
         BNP   DAERR                              IF INPUT IS INVALID           
         CLI   0(R1),11                                                         
         BH    DAERR                                                            
         MVI   AREA,C' '                                                        
         MVC   AREA+1(L'AREA-1),AREA                                            
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   AREA(0),0(R2)                                                    
         MVI   LASTOP,C' '                                                      
         LA    R2,AREA                                                          
         MVI   0(R3),0                                                          
         MVI   0(R8),0                                                          
         MVC   WORKTBL,TABLE                                                    
         LA    R1,WORKTBL                                                       
         ST    R1,LASTA                                                         
         EJECT                                                                  
*              SCAN FOR CHARACTERS                                              
         SPACE 3                                                                
SCAN     LR    R4,R2                                                            
         SPACE 2                                                                
SCAN2    CLI   0(R4),C' '          LOOK FOR ONE OF THE CHARACTERS               
         BE    SCAN4                                                            
         CLI   0(R4),C'/'                                                       
         BE    SCAN4                                                            
         CLI   0(R4),C','                                                       
         BE    SCAN4                                                            
         CLI   0(R4),C'-'                                                       
         BE    SCAN4                                                            
         LA    R4,1(R4)                                                         
         B     SCAN2                                                            
         SPACE 2                                                                
SCAN4    LR    R5,R4               CHECK LENGTH                                 
         SR    R5,R2                                                            
         CH    R5,=H'1'                                                         
         BL    DAERR               CANT BE ZERO                                 
         BH    SCAN6                                                            
         CLI   0(R2),C'T'          IF ITS ONE THEN THE LETTER CANT BE           
         BE    DAERR               T OR S AS THIS IS AMBIGUOUS                  
         CLI   0(R2),C'S'                                                       
         BE    DAERR                                                            
         SPACE 2                                                                
SCAN6    BCTR  R5,0                SET UP FOR TABLE SCAN                        
         L     R6,LASTA                                                         
SCAN7    CLI   0(R6),0             TEST FOR END OF LIST                         
         BE    DAERR                                                            
         SPACE 2                                                                
SCAN8    EX    R5,SCAN10                                                        
         BE    BITOUT                                                           
         LA    R6,5(R6)                                                         
         B     SCAN7                                                            
         SPACE 2                                                                
SCAN10   CLC   0(0,R2),0(R6)                                                    
         EJECT                                                                  
*              NOW OUTPUT THE BITS                                              
         SPACE 3                                                                
BITOUT   MVC   THISDAY,0(R6)                                                    
         CLI   0(R8),0             TEST FOR FIRST TIME                          
         BNE   BIT1                                                             
         IC    R1,4(R6)            START DAY                                    
         SLL   R1,4                                                             
         STC   R1,0(R8)                                                         
         MVI   35(R6),0                                                         
BIT1     OC    0(1,R3),3(R6)                                                    
         CLI   LASTOP,C'-'                                                      
         BNE   BIT4                                                             
         L     R6,LASTA                                                         
         CLI   0(R4),C'-'                                                       
         BE    DAERR                                                            
         SPACE 2                                                                
BIT2     OC    0(1,R3),3(R6)       LOOP TO OUTPUT RANGE OF DAYS                 
         CLC   0(3,R6),THISDAY                                                  
         BE    BIT4                                                             
         LA    R6,5(R6)                                                         
         B     BIT2                                                             
         SPACE 2                                                                
BIT4     CLI   0(R4),C' '          HAVE WE REACHED THE END                      
         BE    DAEXT                                                            
         LA    R6,5(R6)                                                         
         ST    R6,LASTA            NO - STORE THESE VALUES AND RETURN           
         MVC   LASTOP,0(R4)                                                     
         LA    R2,1(R4)                                                         
         B     SCAN                                                             
         SPACE 2                                                                
DAERR    MVI   0(R3),0                                                          
         B     DAEXIT                                                           
         SPACE 2                                                                
DAEXT    OC    0(1,R8),4(R6)                                                    
*                                                                               
DAEXIT   XMOD1 1                                                                
TABLE    DS    0C                                                               
         DC    C'MON'                                                           
         DC    X'4001'                                                          
         DC    C'TUE'                                                           
         DC    X'2002'                                                          
         DC    C'WED'                                                           
         DC    X'1003'                                                          
         DC    C'THU'                                                           
         DC    X'0804'                                                          
         DC    C'FRI'                                                           
         DC    X'0405'                                                          
         DC    C'SAT'                                                           
         DC    X'0206'                                                          
         DC    C'SUN'                                                           
         DC    X'0107'                                                          
         DC    C'MON'                                                           
         DC    X'4001'                                                          
         DC    C'TUE'                                                           
         DC    X'2002'                                                          
         DC    C'WED'                                                           
         DC    X'1003'                                                          
         DC    C'THU'                                                           
         DC    X'0804'                                                          
         DC    C'FRI'                                                           
         DC    X'0405'                                                          
         DC    C'SAT'                                                           
         DC    X'0206'                                                          
         DC    C'SUN'                                                           
         DC    X'0107'                                                          
         DC    X'00'                                                            
         EJECT                                                                  
         EJECT                                                                  
*              DSECT FOR DAYVAL                                                 
         SPACE 3                                                                
DAYD     DSECT                                                                  
AREA     DS    CL32                                                             
LASTA    DS    F                                                                
LASTOP   DS    CL1                                                              
THISDAY  DS    CL3                                                              
WORKTBL  DS    CL70                                                             
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011DDDAYVAL  08/13/00'                                      
         END                                                                    
