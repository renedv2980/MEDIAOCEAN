*          DATA SET CTPRO04    AT LEVEL 011 AS OF 05/01/02                      
*PHASE TA0304A                                                                  
         TITLE 'CTPRO04 - VALIDATE MEDIA SYSTEM PROFILES'                       
PROFMED  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PWRKX-PWRKD,**PR04**,RR=RE                                       
         USING PWRKD,RC                                                         
         ST    RE,RELO                                                          
         L     RF,8(R1)                                                         
         ST    RF,PDATVAL         A(VALDAT)                                     
         LR    R2,R7              PARAMETER LIST                                
         ST    R2,APLIST                                                        
         LA    R2,64(R2)                                                        
         MVC   PNAME(3),0(R2)     PROGRAME NAME                                 
         LA    R3,VALTAB                                                        
READTBL  CLC   PNAME(3),0(R3)     READ THROUGH ROUTINE TABLE FOR ADDR           
         BE    CALLRT             OF VALIDATION ROUTINE                         
         LA    R3,7(R3)                                                         
         CLI   0(R3),X'FF'        END OF TABLE ?                                
         BNE   READTBL                                                          
         SR    R5,R5              NO ROUTINE EXISTS                             
         LTR   R5,R5              SET CONDITION CODE TO 0                       
         B     EXIT                                                             
CALLRT   L     RF,3(R3)           BRANCH TO ROUTINE                             
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
EXIT     XIT1  REGS=(R1)                                                        
***********************************************************************         
* PROCEDURE TO VALIDATE PROFILES FOR PROGRAM ME32                     *         
***********************************************************************         
*                                                                               
         PRINT NOGEN                                                            
RME32    NTR1                                                                   
*                                                                               
         USING PLISTD,R2                                                        
VALPRO   L     R2,APLIST          PROFILE LIST                                  
         LA    R2,16(R2)          POSITION OVER DATE                            
         SR    RF,RF                                                            
         XC    DUB,DUB            CONVERT YEAR TO CHARACTER FORMAT              
         IC    RF,PNEW                                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  YEAR(2),DUB                                                      
*                                                                               
         LA    R2,PNEWLN(R2)                                                    
         SR    RF,RF                                                            
         XC    DUB,DUB                                                          
         IC    RF,PNEW                                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MONTH(2),DUB       CONVERT MONTH TO CHARACTER FORMAT             
         LA    R2,PNEWLN(R2)                                                    
*                                                                               
         SR    RF,RF                                                            
         XC    DUB,DUB                                                          
         IC    RF,PNEW                                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'        CONVERT DAY TO CHARACTER FORMAT               
         UNPK  DAY(2),DUB                                                       
*                                                                               
         L     R8,=F'5'                                                         
         MVC   CKDATE(2),DAY                                                    
         MVC   CKDATE+2(2),MONTH                                                
         MVC   CKDATE+4(2),YEAR                                                 
         CLC   CKDATE(6),=C'000000'                                             
         BE    VALPROX                                                          
         LA    R3,CKDATE                                                        
         GOTO1 PDATVAL,DMCB,(0,(R3)),OUTDAT                                     
         L     R3,DMCB                                                          
         LTR   R3,R3                                                            
         BZ    VALPROA            DATE INVALID                                  
*                                                                               
VALPROX  L     R2,APLIST          PROFILES 8 - 13 SHOULD NOT CONTAIN            
         L     R8,=F'8'                                                         
         L     R3,=F'6'           DUPLICATE VALUES                              
         MVC   CKPARM(24),28(R2)                                                
         LA    R4,CKPARM          R4=A(PROFILES 8 -13)                          
*                                                                               
VALPROB  L     R5,=F'6'           LOOP AROUND 6 TIMES                           
         SR    R7,R7                                                            
         MVC   VALUE,0(R4)                                                      
         LA    R6,CKPARM          R6=A(PROFILES 8 - 13)                         
*                                                                               
VALPROC  CLC   VALUE(4),0(R6)                                                   
         BNE   *+8                                                              
         A     R7,=F'1'           PROFILE MATCHED                               
         LA    R6,4(R6)                                                         
         BCT   R5,VALPROC                                                       
         C     R7,=F'1'           MORE THAN 1 PROFILE MATCHED ?                 
         BH    VALPROA                                                          
         LA    R4,4(R4)                                                         
         A     R8,=F'1'           FOR CURSOR POSITION IF ERROR                  
         BCT   R3,VALPROB                                                       
         SR    R5,R5                                                            
         B     EXIT2                                                            
*                                                                               
VALPROA  L     R5,=F'1'          SET FOR ERROR MESSAGE ON RETURN                
         LR    R1,R8                                                            
         S     R1,=F'1'                                                         
EXIT2    LTR   R5,R5             SET TEST FOR CONDITION CODE                    
         XIT1  REGS=(R1)                                                        
         EJECT                                                                  
         DROP  R2                                                               
*                                                                               
RME29    NTR1                                                                   
         SR    R5,R5                                                            
         LTR   R5,R5                                                            
EXIT29   B     RE                                                               
*                                                                               
RME33    NTR1                                                                   
         SR    R5,R5                                                            
         LTR   R5,R5                                                            
EXIT33   XIT1                                                                   
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
*                                                                               
VALTAB   DS    0XL6                                                             
         DC    XL3'00F3F2',AL4(RME32)                                           
         DC    X'FF'                                                            
*                                                                               
* DSECT TO COVER WORKING STORAGE                                                
*                                                                               
PWRKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PNAME    DS    CL3                                                              
OUTDAT   DS    CL6                                                              
YEAR     DS    CL2                                                              
MONTH    DS    CL2                                                              
DAY      DS    CL2                                                              
CKDATE   DS    CL6                                                              
CKPARM   DS    CL24                                                             
VALUE    DS    CL4                                                              
RELO     DS    A                                                                
VALADDR  DS    A                                                                
APLIST   DS    A                                                                
PDATVAL  DS    A                                                                
PWRKX    EQU   *                                                                
*                                                                               
*DSECT TO COVER PARAMETER LIST                                                  
*                                                                               
PLISTD   DSECT                                                                  
PNEW     DS    CL4                                                              
PNEWLN   EQU   *-PNEW                                                           
PRGNME   DS    CL3                                                              
PACTION  DS    XL1                                                              
PLEVEL   DS    CL1                                                              
POLD     DS    CL4                                                              
POLDLN   EQU   *-POLD                                                           
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011CTPRO04   05/01/02'                                      
         END                                                                    
