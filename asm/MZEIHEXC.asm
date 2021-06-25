*          DATA SET MZEIHEXC   AT LEVEL 119 AS OF 10/30/98                      
*PHASE MZEIHEXC                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
*INCLUDE CARDS                                                                  
         TITLE 'CONVERTING HEX TO DECIMAL'                                      
*                                                                               
TESTFIB  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*TESTFIB,=V(REGSAVE)                                           
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN10                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(TESTFIB),V(DUMMY)                                              
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
REPEAT   DS    0H                                                               
MAIN10   DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   CARD(2),=C'/*'                                                   
         BE    OUT                                                              
         EJECT                                                                  
**********************                                                          
*   YOUR CODE HERE *                                                            
         MVC   TITLE(18),=C'CONVERTING NUMBERS'                                 
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         MVC   P(13),=C'THE INPUT IS:'                                          
         BASR  RE,RF                                                            
         MVC   P(8),CARD                                                        
         BASR  RE,RF                                                            
         MVC   P(15),=C'THE DECIMAL IS:'                                        
         BASR  RE,RF                                                            
*                                                                               
*********SETUP                                                                  
*                                                                               
*        LA    R2,TABLE           R2 IS POINTER TO START OF TABLE               
         LA    R3,CARD            R3 IS POINTER TO HIGH ORDER INPUT             
         SR    R7,R7              STORES VALUE OF INPUT                         
         SR    R8,R8              STORES HEX VALUE OF INPUT                     
         IC    R7,0(R3)           INSERT VALUE OF INPUT INTO R7                 
         SR    R4,R4              MULT REGISTER                                 
         SR    R5,R5              MULT REGISTER                                 
***********                                                                     
*                                                                               
LOOP    DS    0H                                                                
*                                                                               
         CLI   0(R2),X'FF'        END OF TABLE?                                 
         BE    INVALD             IF END, INVALID DATA                          
         CLC   0(,R3),0(R2)       DOES INPUT = VALUE OF TABLE?                  
         BE    MATCH              IF SAME, EXIT LOOP                            
         LA    R2,2(,R2)          MOVE TO NEXT ITEM IN TABLE                    
                                                                                
         B     LOOP               IF NO MATCH, TRY NEXT ITEM                    
*                                                                               
MATCH    DS    0H                                                               
         IC    R8,1(,R2)          INSERT CONVERTED VALUE IN R8                  
         AR    R5,R8              ADD CONVERTED VALUE TO R5                     
*                                                                               
*        LA    R3,1(R3)           INCREMENT INPUT POINTER TO NEXT BYTE          
***************************       BUG HERE                                      
         L     R3,1(R3)           THIS IS THE BUG!!!!!!!!!!!!!!!!!!!!           
         IC    R7,0(,R3)          STORE VAL OF INPUT INTO R7                    
         CLI   0(R3),X'40'        SEE IF BLANK SPACE (END OF INPUT)             
         BE    FINISH             IF SPACE GO TO FINISH                         
*        LA    R2,TABLE           MOVE R2 TO BEGINNING OF CHART                 
         LA    R2,TRTAB                                                         
         M     R4,=F'16'          MULT FOR CONVERSION HERE                      
         B     LOOP                                                             
*                                                                               
***************INVALID                                                          
INVALD  DS     0H                                                               
        MVC    P(20),=C'THE INPUT IS INVALID'                                   
        BASR   RE,RF                                                            
        B      EXITPROG                                                         
*******************                                                             
FINISH   DS    0H                                                               
**********PRINT                                                                 
*                                                                               
         CVD   R5,DEC                                                           
         UNPK  P(8),DEC                                                         
         OI    P+7,X'F0'          CHANGE SIGN BYTE TO F                         
         BASR  RE,RF                                                            
*                                                                               
EXITPROG DS    0H                                                               
**********************                                                          
        B      REPEAT                                                           
*                                                                               
OUT     DS     0H                                                               
*                                                                               
*                                                                               
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
DMCB     DS    6F                                                               
******************************************************                          
* IF YOU WISH TO DEFINE ANY STORAGE OR CONSTANTS,                               
* JUST APPEND THEM TO THE LIST ABOVE (PUT THEM HERE)                            
TRTAB    DC   X'000102030405060708090A0B0C0D0E0FFF'                             
*TABLE   DC   C'0',X'00'                                                        
*        DC   C'1',X'01'                                                        
*        DC   C'2',X'02'                                                        
*        DC   C'3',X'03'                                                        
*        DC   C'4',X'04'                                                        
*        DC   C'5',X'05'                                                        
*        DC   C'6',X'06'                                                        
*        DC   C'7',X'07'                                                        
*        DC   C'8',X'08'                                                        
*        DC   C'9',X'09'                                                        
*        DC   C'A',X'0A'                                                        
*        DC   C'B',X'0B'                                                        
*        DC   C'C',X'0C'                                                        
*        DC   C'D',X'0D'                                                        
*        DC   C'E',X'0E'                                                        
*        DC   C'F',X'0F'                                                        
*        DC   X'FF'                                                             
*END OF TABLE                                                                   
*                                                                               
*INPUT    DC  C'3A42 '                                                          
DEC      DS   D                                                                 
CARD     DS   CL80                                                              
*INPUT    DS   C                                                                
******************************************************                          
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'119MZEIHEXC  10/30/98'                                      
         END                                                                    
