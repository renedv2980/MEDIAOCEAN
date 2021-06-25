*          DATA SET GA2900     AT LEVEL 107 AS OF 08/22/00                      
*PHASE TB2900A                                                                  
FIB29    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 FIBWRKX-FIBWRK,GA2900,RR=R2                                      
************************************************************                    
****************   SET UP  *********************************                    
************************************************************                    
         USING FIBWRK,RC           WORKSPACE BASE ADDRESS                       
         L     RA,4(R1)                                                         
         USING TB29FFD,RA          TWA BASE ADDRESS                             
*                                                                               
         MVC   FIBHEAD,REGMESS                                                  
         OI    FIBHEADH+6,X'80'    TRANSMIT                                     
*                                                                               
         EJECT                                                                  
*************************************************************                   
************  CHECK FOR INPUT, VALIDATE INPUT  **************                   
*************************************************************                   
         CLI   FIBNUMBH+5,0        CHECK FOR INPUT                              
         BNE   INPUT                                                            
         MVC   FIBHEAD,ERRMESS1                                                 
         OI    FIBHEADH+6,X'80'    TRANSMIT                                     
         OI    FIBNUMBH+6,X'40'    CURSOR                                       
         B     ERROR                                                            
INPUT    SR    R4,R4                                                            
         ZIC   R3,FIBNUMBH+5       INPUT LENGTH                                 
         BCTR  R3,0                DECREMENT FOR EX                             
         XC    PACKED,PACKED                                                    
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  PACKED(8),FIBNUMB(0)                                             
         CVB   R4,PACKED           R4 IS NUMBER OF FIB NUMS                     
         C     R4,=F'3'            MUST BE GREATER THAN 3                       
         BNL   CKHIGH                                                           
         MVC   FIBHEAD,ERRMESS2                                                 
         OI    FIBHEADH+6,X'80'    TRANSMIT                                     
         OI    FIBNUMBH+6,X'40'    CURSOR                                       
         B     ERROR                                                            
CKHIGH   C     R4,=F'45'           MUST BE LESS THAN 45                         
         BNH   CKFORM                                                           
         MVC   FIBHEAD,ERRMESS2                                                 
         OI    FIBHEADH+6,X'80'    TRANSMIT                                     
         OI    FIBNUMBH+6,X'40'    CURSOR                                       
         B     ERROR                                                            
         EJECT                                                                  
****************************************************************                
****************   CHECK WHICH FORMAT **************************                
****************************************************************                
CKFORM   CLI   FIBFORM,C'H'                                                     
         BNE   *+8                                                              
         B     FORMHZ                                                           
         CLI   FIBFORM,C'V'                                                     
         BNE   *+8                                                              
         B     FORMVT                                                           
         MVC   FIBHEAD,ERRMESS3                                                 
         OI    FIBHEADH+6,X'80'    TRANSMIT                                     
         OI    FIBFORMH+6,X'40'    CURSOR                                       
         B     ERROR                                                            
         EJECT                                                                  
****************************************************************                
**************** HORIZONTAL FORMAT  ****************************                
****************************************************************                
FORMHZ   SR    R6,R6                                                            
         SR    R7,R7                                                            
         SR    R5,R5                                                            
         LA    R5,1(R5)            SET R5 TO 1                                  
         LA    R6,1(R6)            SET R6 TO 1                                  
         LA    R7,FIBFRSTH         BEGINNING OF OUTPUT FIELDS                   
         OI    6(R7),X'80'         TRANSMIT                                     
         LA    R7,8(R7)            HARD BUMP TO DATA FIELD                      
         EDIT  (R5),(15,(R7)),COMMAS=YES                                        
         BCTR  R4,0                COUNTER                                      
         LA    R7,15(R7)           HARD BUMP TO NEXT HEADER                     
         OI    6(R7),X'80'         TRANSMIT                                     
         LA    R7,8(R7)            HARD BUMP TO DATA FIELD                      
         EDIT  (R6),(15,(R7)),COMMAS=YES                                        
         BCTR  R4,0                COUNTER                                      
*************** NOW CAN START LOOP  ********************                        
LOOPHZ   AR    R5,R6               CALCULATE NEXT FIB NUMBER                    
         LA    R7,15(R7)           HARD BUMP TO NEXT HEADER                     
         OI    6(R7),X'80'         TRANSMIT                                     
         LA    R7,8(R7)            HARD BUMP TO DATA FIELD                      
         EDIT  (R5),(15,(R7)),COMMAS=YES                                        
         XR    R5,R6               SWAP THE TWO REGISTERS                       
         XR    R6,R5                                                            
         XR    R5,R6                                                            
         BCT   R4,LOOPHZ                                                        
         B     ERROR                                                            
*************************************************************                   
***************   VERTICAL FORMAT  **************************                   
*************************************************************                   
FORMVT   SR    R6,R6                                                            
         SR    R7,R7                                                            
         SR    R5,R5                                                            
         LA    R5,1(R5)            SET R5 TO 1                                  
         LA    R6,1(R6)            SET R6 TO 1                                  
         LA    R7,FIBFRSTH         BEGINNING OF OUTPUT FIELDS                   
         OI    6(R7),X'80'         TRANSMIT                                     
         LA    R7,8(R7)            HARD BUMP TO DATA FIELD                      
         EDIT  (R5),(15,(R7))                                                   
         BCTR  R4,0                COUNTER                                      
         LA    R7,15(R7)           HARD BUMP TO NEXT HEADER                     
*                                                                               
         OI    FIBNUMBH+6,X'40'    CURSOR                                       
         B     ERROR                                                            
*                                                                               
ERROR    XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
***************    CONSTANTS   ******************************                   
*************************************************************                   
REGMESS  DC    CL60'FIBONACCI SEQUENCE'                                         
ERRMESS1 DC    CL60'PLEASE ENTER REQUIRED FIELDS'                               
ERRMESS2 DC    CL60'NUMBER MUST BE IN RANGE 3-45'                               
ERRMESS3 DC    CL60'FORMAT MUST BE H OR V'                                      
************************************************************                    
**************   WORKSPACE    ******************************                    
************************************************************                    
FIBWRK   DSECT                                                                  
PACKED   DS    D                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    CL17                                                             
FIBWRKX  EQU   *                                                                
         EJECT                                                                  
************************************************************                    
********************    TWA    *****************************                    
************************************************************                    
       ++INCLUDE GA29FFD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'107GA2900    08/22/00'                                      
         END                                                                    
