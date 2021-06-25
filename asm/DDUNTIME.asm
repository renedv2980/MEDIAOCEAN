*          DATA SET DDUNTIME   AT LEVEL 027 AS OF 05/01/02                      
*PHASE T00A11A                                                                  
*===========>>   WAS CATALP UNTIME MHER 07MAY01                                 
         TITLE 'TIME CONVERSION MODULE - 4BYTE TO VARIABLE OUT'                 
*                                                                               
*                                                                               
*                          *****************                                    
*                          * UNTIME MODULE *                                    
*                          *****************                                    
*  ADDED SUPPORT OF MILITARY TIME OUTPUT                                        
*                                                                               
UNTIMEC  CSECT                                                                  
         ENTRY UNTIME                                                           
         PRINT NOGEN                                                            
UNTIME   NMOD1 5,UNTIME                                                         
         L     R5,0(R1)                  ADDRESS OF 4 BYTE TIME                 
         L     R6,4(R1)                  ADDRESS OF OUTPUT AREA                 
         SR    R7,R7                                                            
         ICM   R7,7,9(R1)                A(=C'MIL')                             
         USING PTKYD,RC                                                         
         MVC   KEYZONE,0(R1)                                                    
*                                                                               
         CLC   0(4,R5),=C'VARY'                                                 
         BNE   *+14                                                             
         MVC   0(7,R6),=C'VARIOUS'                                              
         B     UNTEXT                                                           
*                                                                               
         CLC   0(4,R5),=C'NONE'                                                 
         BNE   *+14                                                             
         MVC   0(4,R6),=C'NONE'                                                 
         B     UNTEXT                                                           
         MVC   INPSAVE(4),0(R5)                                                 
         NC    INPSAVE(4),=X'0FFF0FFF' STRIP FLAGS                              
*                                                                               
         CLC   =C'MIL',0(R7)                                                    
         BNE   UNT1                                                             
         SR    R0,R0                                                            
         ICM   R0,3,INPSAVE        LOAD FIRST 2 BYTE TIME                       
         BAS   RE,CHKZONE                                                       
         SR    R0,RF               ADJUST ZONE SETTING                          
         CVD   R0,KEYDUB                                                        
         UNPK  0(4,R6),KEYDUB                                                   
         OI    3(R6),X'F0'                                                      
         MVC   DMCB(2),2(R6)       TEMOPARILY SAVE MINUTES                      
         MVI   2(R6),C':'          SEPERATOR BET. HOUR AND MIN.                 
         MVC   3(2,R6),DMCB        MOVE BACK MINUTES                            
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,INPSAVE+2      LOAD SECOND 2 BYTE TIME                      
         BZ    UNTEXT                                                           
         BAS   RE,CHKZONE                                                       
         SR    R0,RF                                                            
         CVD   R0,KEYDUB                                                        
         UNPK  6(4,R6),KEYDUB                                                   
         OI    9(R6),X'F0'                                                      
         MVI   5(R6),C'-'                                                       
         MVC   DMCB(2),8(R6)       TEMOPARILY SAVE MINUTES                      
         MVI   8(R6),C':'          SEPERATOR BET. HOUR AND MIN.                 
         MVC   9(2,R6),DMCB        MOVE BACK MINUTES                            
         B     UNTEXT                                                           
*                                                                               
UNT1     SR    R0,R0                                                            
         ICM   R0,3,INPSAVE                                                     
         BAS   RE,CHKZONE                                                       
         SR    R0,RF                                                            
         CH    R0,=H'2400'                                                      
         BNH   *+8                                                              
         SH    R0,=H'2400'                                                      
         CVD   R0,KEYDUB                                                        
         CP    KEYDUB,=PL3'1200'                                                
         BC    10,UNT2                                                          
         MVI   KEYDUB+8,C'A'       AM                                           
         CP    KEYDUB,=PL2'100'                                                 
         BC    10,UNT4                                                          
         AP    KEYDUB,=PL3'1200'                                                
         B     UNT4                                                             
UNT2     MVI   KEYDUB+8,C'P'       PM                                           
         BC    2,*+8                                                            
         MVI   KEYDUB+8,C'N'       NOON                                         
         CP    KEYDUB,=PL3'2400'                                                
         BNE   *+8                                                              
         MVI   KEYDUB+8,C'M'       MIDNIGHT                                     
         CP    KEYDUB,=PL3'1300'                                                
         BC    4,UNT4                                                           
         SP    KEYDUB,=PL3'1200'                                                
         EJECT                                                                  
UNT4     UNPK  0(4,R6),KEYDUB      TIME                                         
         OI    3(R6),X'F0'                                                      
         LA    RE,2(R6)            POINT TO LAST 2 CHARACTERS                   
         CLI   0(R6),C'0'          TEST LEADING 0                               
         BNE   UNT4A                                                            
         MVC   0(3,R6),1(R6)                                                    
         MVI   3(R6),C' '                                                       
         BCTR  RE,0                ADJUST POINTER                               
UNT4A    ZAP   FIRST,KEYDUB        SAVE FIRST                                   
         CLC   0(2,RE),=C'00'                                                   
         BNE   UNT4B                                                            
         MVC   0(2,RE),=C'  '                                                   
         BCTR  RE,0                                                             
         BCTR  RE,0                                                             
UNT4B    MVC   2(1,RE),KEYDUB+8    A OR F                                       
*                                                                               
UNT5     LA    R6,3(RE)            NEXT OUTPUT POSITION                         
         CLC   2(2,R5),=C'CC'      CHECK FOR CONCLUSION                         
         BC    7,UNT7                                                           
         MVC   0(3,R6),=C'-CC'     MOVE MESSAGE AND EXIT                        
         B     UNTEXT                                                           
UNT7     SR    R0,R0                                                            
         ICM   R0,3,INPSAVE+2                                                   
         BZ    UNTEXT                                                           
         BAS   RE,CHKZONE                                                       
         SR    R0,RF                                                            
         CH    R0,=H'2400'                                                      
         BNH   *+8                                                              
         SH    R0,=H'2400'                                                      
         CVD   R0,KEYDUB                                                        
         CP    KEYDUB,=PL3'1200'                                                
         BC    10,UNT8                                                          
         MVI   KEYDUB+9,C'A'       AM                                           
         CP    KEYDUB,=PL2'100'                                                 
         BC    10,UNT9                                                          
         AP    KEYDUB,=PL3'1200'                                                
         B     UNT9                                                             
         EJECT                                                                  
UNT8     MVI   KEYDUB+9,C'P'       PM                                           
         BC    2,*+8                                                            
         MVI   KEYDUB+9,C'N'       NOON                                         
         CP    KEYDUB,=PL3'1300'                                                
         BC    4,UNT9                                                           
         SP    KEYDUB,=PL3'1200'                                                
         CP    KEYDUB,=PL3'1200'                                                
         BC    7,UNT9                                                           
         MVI   KEYDUB+9,C'M'       MIDNIGHT                                     
*                                                                               
UNT9     CLC   KEYDUB+8(1),KEYDUB+9                                             
         BC    7,UNTA                                                           
         CP    FIRST,KEYDUB        IF 1ST IS HIGH NEED A                        
         BH    UNTA                                                             
         BCTR  R6,0                AM/PM SAME ELIMINATE FIRST                   
*                                                                               
UNTA     MVI   0(R6),C'-'                                                       
         UNPK  1(4,R6),KEYDUB      END TIME                                     
         LA    6,1(R6)                                                          
         OI    3(R6),X'F0'                                                      
         MVC   4(1,R6),KEYDUB+9    A OR F                                       
         CLI   0(R6),X'F0'                                                      
         BC    7,UNTB                                                           
         MVC   0(4,R6),1(R6)                                                    
         MVI   4(R6),X'40'                                                      
         BCTR  R6,0                                                             
*                                                                               
UNTB     CLI   2(R6),X'F0'                                                      
         BC    7,UNTEXT                                                         
         CLI   3(R6),X'F0'                                                      
         BC    7,UNTEXT                                                         
         MVC   2(1,R6),4(R6)                                                    
         MVI   3(R6),X'40'                                                      
         MVI   4(R6),X'40'                                                      
UNTEXT   DS    0H                                                               
UNXIT    XIT1                                                                   
*                                                                               
CHKZONE  SR    RF,RF                                                            
         CLI   KEYZONE,C'C'                                                     
         BNE   CHKZN10                                                          
         LA    RF,100                                                           
CHKZN10  CLI   KEYZONE,C'M'                                                     
         BNE   CHKZN20                                                          
         LA    RF,200                                                           
CHKZN20  CLI   KEYZONE,C'P'                                                     
         BNE   CHKZN30                                                          
         LA    RF,300                                                           
CHKZN30  BR    RE                                                               
*                                                                               
DMCB     DS    6F                                                               
         LTORG                                                                  
PTKYD    DSECT                                                                  
KEYDUB   DS    CL8                                                              
SUBPAR1  DS    CL4                                                              
SUBPAR2  DS    CL4                                                              
FIRST    DS    PL8                                                              
KEYZONE  DS    CL1                                                              
INPSAVE  DS    CL4                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027DDUNTIME  05/01/02'                                      
         END                                                                    
