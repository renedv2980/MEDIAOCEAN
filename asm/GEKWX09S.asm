*          DATA SET GEKWX09S   AT LEVEL 006 AS OF 04/10/91                      
*PHASE TF2009A,*                                                                
         TITLE 'GEKWX09 - TF2009 - $KWX ERROR HANDLING OVERLAY'                 
*                                                                               
*********************************************************************           
*                                                                   *           
*   GEKWX09 - TF2009 - KWX, ERROR HANDLING                          *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* AUG30/89 (MRR) --- TEMP CHANGE MORE THAT 100 DEST MESSAGE TO      *           
*                     'TOO MANY'.                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
KWX09    CSECT                                                                  
         NMOD1 0,**KWX09*                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         USING TWAD,RA                                                          
         MVC   KWXHEAD,SPACES                                                   
         MVC   KWXHEAD(21),=CL21'*** ERROR *** UNKNOWN'                         
         ZIC   R4,FERN                                                          
         BCTR  R4,0                                                             
         MH    R4,=H'30'                                                        
         LA    R4,ERRLST(R4)                                                    
         LA    RF,ENDLST                                                        
         CR    R4,RF                                                            
         BNL   EXIT                                                             
         MVC   KWXHEAD+14(30),0(R4)                                             
         CLI   FNDX,0                                                           
         BE    EXIT                                                             
         LA    R4,KWXHEAD+59                                                    
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVC   2(6,R4),=C'- FLD#'                                               
T2       EDIT  FNDX,(2,8(R4)),ALIGN=LEFT                                        
         SPACE 1                                                                
T3       CLI   SUBFNDX,0           SUBFIELD NUMBER                              
         BE    EXIT                                                             
         AR    R4,R0                                                            
         MVI   8(R4),C'.'                                                       
         LA    R4,1(R4)                                                         
         MVC   FNDX,SUBFNDX                                                     
         MVI   SUBFNDX,0                                                        
         B     T2                                                               
EXIT     XMOD1 1                                                                
         SPACE 1                                                                
ERRLST   DS    0CL30                                                            
         DC    CL30'MISSING INPUT FIELD'                         1              
         DC    CL30'INVALID INPUT FIELD'                         2              
         DC    CL30'NO SUCH USER OR LIST EXISTS'                 3              
         DC    CL30'LIST NESTED TOO DEEP'                        4              
         DC    CL30'TOO MANY INPUT FIELDS'                       5              
         DC    CL30'NO ADDRESSEES'                               6              
         DC    CL30'SUBSTITUTION INVALID'                        7              
         DC    CL30'NO SUBSTITUTION HITS'                        8              
         DC    CL30'NOT ENOUGH ROOM ON SCREEN'                   9              
         DC    CL30'REPORT DOES NOT EXIST'                      10              
         DC    CL30'TEXT DOES NOT EXIST'                        11              
         DC    CL30'KEYWORD INVALID'                            12              
         DC    CL30'MESSAGE SIZE EXCEEDS MAXIMUM'               13              
         DC    CL30'NOT LOGGED ON'                              14              
         DC    CL30'SEPARATE ADDRESSEES WITH +/-'               15              
         DC    CL30'NOT THE SENDER''S COPY OF A KWX'            16              
         DC    CL30'NO ''REPORT='' ON PRECEDING LINE'           17              
         DC    CL30'SINGLE PAGE MUST BE SPECIFIED'              18              
         DC    CL30'NOT NUMERIC (N OR N1-N2)'                   19              
         DC    CL30'START GREATER THAN END'                     20              
         DC    CL30'NUMBER EXCEEDS MAXIMUM'                     21              
*        DC    CL30'MORE THAN 100 ADDRESSEES'                                   
         DC    CL30'TOO MANY ADDRESSES'                         22              
         DC    CL30'ACTION INCOMPATIBLE WITH MODE'              23              
         DC    CL30'BOOK NOT YET SPECIFIED'                     24              
         DC    CL30'ACCESS NOT AUTHORIZED'                      25              
         DC    CL30'DUPLICATE PARAMETER'                        26              
         DC    CL30'CLOSE BRACKET (>) MISSING'                  27              
         DC    CL30'INVALID NUMBER'                             28              
         DC    CL30'INVALID INPUT FOR AMENDMENT'                29              
         DC    CL30'FORMAT NOT FOUND'                           30              
         DC    CL30'SCREEN ERROR, PLEASE RESPECIFY'             31              
         DC    CL30'TERMINATOR /&& MISSING'                     32              
         DC    CL30'INVALID RULE LETTER'                        33              
         DC    CL30'FIELD TERMINATOR " MISSING'                 34              
         DC    CL30'FORMAT RECORD TOO LONG'                     35              
         DC    CL30'INVALID CASH'                               36              
         DC    CL30'INVALID DATE'                               37              
         DC    CL30'QUALIFIERS INVALID FOR ACTION'              38              
         DC    CL30'INPUT FIELD TOO LONG'                       39              
         DC    CL30'BOOK DOES NOT EXIST'                        40              
         DC    CL30'NO MORE BOOK NUMBERS AVAILABLE'             41              
         DC    CL30'RECORD NOT FOUND'                           42              
         DC    CL30'DISK ERROR - PLEASE CALL DDS'               43              
         DC    CL30'END OF FILE - NO MORE SPACE'                44              
         DC    CL30'YOU ARE ALREADY IN THIS MODE'               45              
         DC    CL30'INCOMPATIBLE PARAMETERS'                    46              
         DC    CL30'INVALID FUNCTION FOR NON-OWNER'             47              
         DC    CL30'NO ROOM IN BOOK'                            48              
         DC    CL30'ALREADY EXISTS'                             49              
         DC    CL30'BOOK HAS BEEN ENDED - UNEND ?'              50              
         DC    CL30'BOOK ALREADY EXISTS'                        51              
         DC    CL30'NUMBER TOO HIGH'                            52              
         DC    CL30'BOOK NOT YET ENDED TO CHECKSUM'             53              
         DC    CL30'NO INPUT'                                   54              
         DC    CL30'POINT OF INSERT NOT DISPLAYED'              55              
         DC    CL30'REF NUMBER IS BEYOND BOOK END'              56              
         DC    CL30'MESSAGE NOT YET DISPLAYED'                  57              
         DC    CL30'NO CHANGES HAVE BEEN MADE'                  58              
         DC    CL30'ACTION NOT AVAILABLE'                       59              
         DC    CL30'INVALID &&EOL - MUST FOLLOW FLD'            60              
         DC    CL30'MUST BE AT LEAST 3 ALPHA CHARS.'            61              
         DC    CL30'INVALID BRACKETS - USE < AND >'             62              
         DC    CL30'ADD AFTER LAST NOT ALLOWED'                 63              
         DC    CL30'SAME BOOKNAME AS ORIGINAL'                  64              
         DC    CL30'INITIALS INVALID FOR FORMAT'                65              
         DC    CL30'ONLY ACE/TWX STATIONS ALLOWED'              66              
         DC    CL30'MUST USE ''FORMAT=TWX'''                    67              
*                                                                               
ENDLST   DS    0C                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
* GEKWXDSECT                                                                    
         PRINT OFF                                                              
       ++INCLUDE GEKWXDSECT                                                     
         PRINT ON                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006GEKWX09S  04/10/91'                                      
         END                                                                    
