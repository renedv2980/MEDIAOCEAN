*          DATA SET DDINVEDIT  AT LEVEL 011 AS OF 03/20/07                      
*PHASE T00A09A                                                                  
         TITLE 'INVEDIT - EDITS INVENTORY NOS. TO DAY/TIME'                     
INVEDIT  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 INVL,*INVED**                                                    
         USING INV,RC                                                           
         LM    R2,R3,0(R1)         A(2 BYTE INV NO.)                            
*                                       A(13 BYTE AREA) (DDDHH.MMAIIII)         
*                                                                               
         SPACE 3                                                                
         ZIC   R4,1(R2)            DAY IS IN BYTE 2 HIGH NIBBLE                 
         SRL   R4,4                                                             
         STC   R4,11(R3)           SHOW DAY CODE IN INVENTORY NO.               
         CLI   11(R3),10           ALLOW DAY CODES ABOVE 9                      
         BL    NUMDAY                                                           
         ZIC   R0,11(R3)           BUT CONVERT THEM TO CHARACTERS               
         SH    R0,=H'9'                                                         
         STC   R0,11(R3)                                                        
         OI    11(R3),X'C0'        CHANGE TO A LETTER                           
         B     *+8                                                              
NUMDAY   OI    11(R3),X'F0'        CHANGE TO AN NUMBER                          
         MH    R4,=H'3'                                                         
         LA    R4,DAYTAB(R4)                                                    
         MVC   0(3,R3),0(R4)                                                    
         SPACE 2                                                                
         ZIC   R4,0(R2)            1/4 HOUR IS IN BYTE 1                        
         CVD   R4,DUB              SHOW QUARTER HOUR CODE                       
         UNPK  9(2,R3),DUB                                                      
         OI    10(R3),X'F0'                                                     
         SRL   R4,2                (NOW HAVE HOURS)                             
         MH    R4,=H'3'                                                         
         LA    R4,HOURTAB(R4)                                                   
         MVC   3(2,R3),0(R4)       OUTPUT HH                                    
         MVI   5(R3),C'.'                 .                                     
         MVC   8(1,R3),2(R4)              A/P                                   
         SPACE 2                                                                
         ZIC   R4,0(R2)                                                         
         SLL   R4,30                                                            
         SRL   R4,29                                                            
         LA    R4,MINTAB(R4)       OUTPUT MM                                    
         MVC   6(2,R3),0(R4)                                                    
         CLC   3(5,R3),=C'12.00'   ADJUST FOR NOON AND MIDNIGHT                 
         BNE   NUM                                                              
         CLI   8(R3),C'P'                                                       
         BNE   *+12                                                             
         MVI   8(R3),C'N'                                                       
         B     NUM                                                              
         MVI   8(R3),C'M'                                                       
         SPACE 2                                                                
NUM      ZIC   R4,1(R2)                                                         
         SLL   R4,28                                                            
         SRL   R4,29                                                            
         STC   R4,12(R3)                                                        
         OI    12(R3),X'F0'                                                     
         CLI   12(R3),X'F0'                                                     
         BNE   *+8                                                              
         MVI   12(R3),C' '                                                      
         TM    1(R2),X'01'                                                      
         BNO   XIT                                                              
         NI    12(R3),X'CF'        CHANGE 1-7 INTO A-G                          
         CLI   12(R3),C' '         WITH THE EXCEPTION OF SPACE                  
         BNE   XIT                                                              
         MVI   12(R3),X'F0'        WHICH BECOMES ZERO                           
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
DAYTAB   DC    C'M-FMONTUEWEDTHUFRISATSUNM-SVAR'                                
         DC    18C'?????????TYPWKE???'                                          
HOURTAB  DC    C' 6A 7A 8A 9A10A11A12P 1P 2P 3P 4P 5P'                          
         DC    C' 6P 7P 8P 9P10P11P12A 1A 2A 3A 4A 5A'                          
MINTAB   DC    C'00153045'                                                      
INV      DSECT                                                                  
DUB      DS    D                                                                
INVL     EQU   *-INV                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011DDINVEDIT 03/20/07'                                      
         END                                                                    
