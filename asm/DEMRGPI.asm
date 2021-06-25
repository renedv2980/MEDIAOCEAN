*          DATA SET DEMRGPI    AT LEVEL 006 AS OF 04/16/08                      
*PHASE DEMRGIA                                                                  
         TITLE 'DEMO FILE MERGE INPUT PHASE'                                    
DEMRGI   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*DEMRGI*                                                       
         USING DEMCOND,R8                                                       
         L     RC,ARREC                                                         
         LA    RC,4(RC)                                                         
         USING PRKEY,RC                                                         
         L     R2,AIREC                                                         
         USING INTERD,R2                                                        
         B     *+4(R1)                                                          
         SPACE 2                                                                
         B     READ                GET INPUT (ARREC - INT)                      
         B     CNVWR               CONVERT TO OUTPUT (INT - WORK)               
         B     MORET               CLOSE INPUT                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*===================== GET INPUT (RREC --> IREC) =====================*         
*                                                                               
READ     CLI   INTAPESW,1                                                       
         BE    OPENOK                                                           
         OPEN  (IN1,(INPUT))                                                    
*                                                                               
OPENOK   L     R4,ARREC                                                         
         L     RE,ARREC                                                         
         LA    RF,2000                                                          
         XCEF                                                                   
         GET   IN1,(R4)                                                         
*                                                                               
         L     RE,ANIINPUT         SET FOR RECORD COUNTS                        
         L     R0,0(RE)                                                         
         AHI   R0,1                                                             
         ST    R0,0(RE)                                                         
*                                                                               
         MVC   INTKEY(PRRLEN-PRCODE),PRCODE                                     
         LH    R1,0(R4)                                                         
         LA    RF,INTDATA                                                       
         LA    RE,PRCODE                                                        
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         LH    RF,0(R4)                                                         
         LA    RF,50(RF)                                                        
         STH   RF,INTRECLN                                                      
         MVI   INTRTYP,PRCODEQU                                                 
*                                                                               
         BAS   RE,SETKEY                                                        
         B     EXIT                                                             
         DROP  RC                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*---------------------------- SORT RECORD ----------------------------*         
*                                                                               
SETKEY   NTR1                 BUILD SORT KEY                                    
         L     R6,AIREC                                                         
         USING PRKEY,R6                                                         
         LA    R6,4(R6)                                                         
         DROP  R6                                                               
         XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*              SORT RECORD PROCESSING FOR KEY SEQUENCING                        
*                                                                               
CNVWR    DS    0H                                                               
         L     R6,ASREC            POINT AT SORT RECORD                         
         LR    R2,R6               ADDRESSABILITY FOR INTERIM VALUES            
         LA    R6,4(R6)            POINT AT RECORD START                        
         USING PRKEY,R6                                                         
         SPACE 1                                                                
CNVWR2   MVC   PREVKEY,PRKEY       UPDATE PREVIOUS KEY                          
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
MORET    DS    0H                                                               
ENDJOB   CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XMOD1                                                                  
         EJECT                                                                  
PREVKEY  DC    XL20'00'                                                         
         SPACE 3                                                                
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=RRECL,                                            X        
               MACRF=GM,                                               X        
               EODAD=MORET                                                      
RRECL    EQU   2000                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DEDEMCNVD                                                      
         EJECT                                                                  
*============================= IREC DSECT ============================*         
       ++INCLUDE DEINTD                                                         
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006DEMRGPI   04/16/08'                                      
         END                                                                    
