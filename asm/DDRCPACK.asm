*          DATA SET DDRCPACK   AT LEVEL 005 AS OF 11/01/02                      
*PHASE T00ABCA                                                                  
                                                                                
*=========================================================                      
* R1 --> C'P',AL3(REPCODE)                                                      
*        A(2 BYTE OUTPUT)                                                       
*        X'80' = ALLOW ANY ALPHANUMERICS                                        
*                                                                               
*        C'U',AL3(2-BYTE REPCODE)                                               
*        A(3 BYTE OUTPUT)                                                       
*                                                                               
* PROGRAM RETURNS CC NEQ IF CODE INVALID                                        
*                                                                               
*=========================================================                      
                                                                                
RCPACK   TITLE 'ALPHANUMERIC REPCODES TO 2 BYTE BINARY'                         
RCPACK   CSECT                                                                  
         NMOD1 WORKX-WORKD,*RCPACK*,CLEAR=YES                                   
         USING WORKD,RC                                                         
*                                                                               
         ST    R1,APARMS                                                        
         SR    R2,R2                                                            
         ICM   R2,7,1(R1)                                                       
         SR    R3,R3                                                            
         ICM   R3,7,5(R1)                                                       
*                                                                               
         CLI   0(R1),C'P'                                                       
         BE    PACK                                                             
         CLI   0(R1),C'U'                                                       
         BE    UNPK                                                             
         DC    H'0'                                                             
*                                                                               
PACK     CLI   0(R2),C' '          FIRST CHAR CANNOT BE A SPACE                 
         BNH   NEQXIT                                                           
         CLI   2(R2),C' '          IF THIRD CHAR PRESENT                        
         BNH   *+12                IT'S NOT                                     
         CLI   1(R2),C' '          THEN SECOND CANNOT BE A SPACE                
         BNH   NEQXIT                                                           
*                                                                               
         MVC   FULL(3),0(R2)                                                    
         TR    FULL(3),PACKTAB                                                  
*                                                                               
         TM    4(R1),X'80'         TEST ALWAYS TRANSLATE                        
         BO    PACK20              YES                                          
*                                                                               
         LA    R4,0(R2)            POINT TO ORIGINAL INPUT STRING               
         LA    R5,3                SEE IF INPUT IS ALL NUMERIC                  
         CLI   0(R4),C'0'          ALPHA REPS CANNOT START WITH NUMBER          
         BL    PACK20                                                           
*                                                                               
PACK2    CLI   0(R4),C' '          COMPARE TO C' '                              
         BE    PACK4                                                            
         CLI   0(R4),C'0'          MUST BE NUMERIC                              
         BL    NEQXIT                                                           
         CLI   0(R4),C'9'                                                       
         BH    NEQXIT                                                           
*                                                                               
PACK4    LA    R4,1(R4)                                                         
         BCT   R5,PACK2                                                         
*                                                                               
PACK6    LHI   R5,2                SET R5 FOR EX                                
         CLI   FULL+2,0            TEST TRAILING SPACE                          
         BNE   PACK10              NO                                           
         BCTR  R5,0                                                             
         CLI   FULL+1,0                                                         
         BNE   PACK10                                                           
         BCTR  R5,0                                                             
*                                                                               
PACK10   EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         CVB   R0,DUB                                                           
         STH   R0,0(R3)                                                         
*                                                                               
EQXIT    CR    RB,RB               SET CC EQ                                    
         B     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
* CHECK FOR VALID CHARS                                                         
*                                                                               
PACK20   LA    R0,3                                                             
         LA    R1,FULL                                                          
*                                                                               
PACK22   CLI   0(R1),X'FF'                                                      
         BE    NEQXIT                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,PACK22                                                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FULL                                                          
         MHI   RF,37*37                                                         
         LR    RE,RF                                                            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FULL+1                                                        
         MHI   RF,37                                                            
         AR    RE,RF                                                            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FULL+2                                                        
         AR    RE,RF                                                            
*                                                                               
         STH   RE,0(R3)                                                         
         B     EQXIT                                                            
         EJECT                                                                  
UNPK     SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,0(R2)                                                       
         CHI   RF,1000                                                          
         BH    UNPK10                                                           
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R3),DUB                                                      
         B     EQXIT                                                            
*                                                                               
UNPK10   D     RE,=F'37'           REMAINDER=3RD CHAR OF STRING                 
         IC    RE,UNPKTAB(RE)                                                   
         STC   RE,2(R3)                                                         
*                                                                               
         SR    RE,RE                                                            
         D     RE,=F'37'           REMAINDER=2ND CHAR OF STRING                 
         IC    RE,UNPKTAB(RE)                                                   
         STC   RE,1(R3)                                                         
*                                                                               
         IC    RF,UNPKTAB(RF)      QUOTIENT=1ST CHAR OF STRING                  
         STC   RF,0(R3)                                                         
         B     EQXIT                                                            
*                                                                               
PACKTAB  DC    16X'FF'             X'00'                                        
         DC    16X'FF'             X'10'                                        
         DC    16X'FF'             X'20'                                        
         DC    16X'FF'             X'30'                                        
         DC    X'00',15X'FF'       X'40'                                        
         DC    16X'FF'             X'50'                                        
         DC    16X'FF'             X'60'                                        
         DC    16X'FF'             X'70'                                        
         DC    16X'FF'             X'80'                                        
         DC    16X'FF'             X'90'                                        
         DC    16X'FF'             X'A0'                                        
         DC    16X'FF'             X'B0'                                        
         DC    X'FF010203040506070809FFFFFFFFFFFF' X'C0'                        
         DC    X'FF0A0B0C0D0E0F101112FFFFFFFFFFFF' X'D0'                        
         DC    X'FFFF131415161718191AFFFFFFFFFFFF' X'E0'                        
         DC    X'1B1C1D1E1F2021222324FFFFFFFFFFFF' X'F0'                        
*                                                                               
UNPKTAB  DC    C' ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'                         
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
APARMS   DS    A                                                                
FULL     DS    A                                                                
DUB      DS    D                                                                
WORKX    EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005DDRCPACK  11/01/02'                                      
         END                                                                    
