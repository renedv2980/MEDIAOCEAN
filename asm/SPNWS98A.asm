*          DATA SET SPNWS98A   AT LEVEL 001 AS OF 06/25/92                      
*PHASE T20798A,*                                                                
         TITLE 'BWS98 - BUYERS WORK SHEET - FIX WORK RECORDS'                   
T20798   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20798**,RA,RR=RE                                              
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(SAVE AREA)                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
*                                                                               
         MVC   APPARM,COMPARM                                                   
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         DC    AL4(0)  VALKEY                                                   
         DC    AL4(0)  VALREC                                                   
         DC    AL4(0)  DISKEY                                                   
         DC    AL4(0)  DISREC                                                   
         DC    AL4(0)  DELREC                                                   
         DC    AL4(0)  RESREC                                                   
         B     VALPAR                                                           
         DC    AL4(0)  GETSEL                                                   
         DC    AL4(0)  DISSEL                                                   
         DC    AL4(0)  VALSEL                                                   
         B     EXIT    FSTLST                                                   
         DC    AL4(0)                                                           
         DC    AL4(0)  FSTSCR                                                   
         DC    AL4(0)  LASSCR                                                   
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT PARAMETERS                                          *         
* OUTPUT : RECORDS WITH BAD STATION SEQUENCE NUMBER ARE DELETED       *         
*          AND ADDED WITH CORRECT SEQUENCE NUMBER IF NOT A PACKAGE    *         
*          OR ORBIT.                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALPAR   GOTO1 AVALPARM,BWSKY2H    VALIDATE SELECT PARAMETERS                   
         BNE   VALPX                                                            
         TM    TWAFLAG,TWANODET    TEST FOR NO DETAIL RECORDS                   
         BNZ   VALPX                                                            
*                                                                               
         MVI   CHANGE,C'N'                                                      
         L     R2,AIOAREA1                                                      
         USING BWHRECD,R2                                                       
         LA    R0,BWHMAXST                                                      
         LA    R4,BWHSTA                                                        
         LA    R8,1                                                             
         MVI   BSTACD,0                                                         
*                                                                               
VALP0    CLC   0(5,R4),=C'WOODT'                                                
         BNE   VALP0A                                                           
         CLI   BSTACD,0                                                         
         BNE   VALP0B                                                           
         STC   R8,BSTACD                                                        
*                                                                               
VALP0A   LA    R4,L'BWHSTA(R4)                                                  
         LA    R8,1(R8)                                                         
         BCT   R0,VALP0                                                         
         B     VALP90                                                           
*                                                                               
VALP0B   LA    R3,IOKEY                                                         
         USING BWDRECD,R3                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(13),DTLKEY                                                 
*                                                                               
VALP1    LA    R1,MINHI2                                                        
         B     VALP2+4                                                          
*                                                                               
VALP2    LA    R1,MINSEQ2          READ ALL DETAIL RECORDS                      
         GOTO1 AMIN                                                             
         BNE   VALP90                                                           
         CLC   IOKEY(BWDKELST-BWDKEY),IOKEYSAV                                  
         BNE   VALP90                                                           
         L     R3,AIOAREA2                                                      
         CLC   BWDSTA,=C'WOODT'    TEST WOOD                                    
         BNE   VALP2                                                            
         CLC   BWDSTACD,BSTACD     YES-TEST CORRECT STATION CODE                
         BE    VALP2                                                            
         MVI   CHANGE,C'Y'                                                      
         GOTO1 AMIN,MINDEL         NO-DELETE RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   BWDKELPO,0          TEST PACKAGE/ORBIT                           
         BNE   VALP2               YES-READ NEXT RECORD                         
*                                                                               
         MVC   APRECKEY(13),IOKEY                                               
         LA    R3,IOKEY                                                         
         MVC   BWDKELST,BSTACD     CORRECT STATION SEQUENCE NUMBER              
         MVI   BWDKELSQ,0                                                       
         SR    R9,R9                                                            
         LA    R1,MINHI3                                                        
         B     VALP8+4                                                          
*                                                                               
VALP8    LA    R1,MINSEQ3                                                       
         GOTO1 AMIN                                                             
         BNE   VALP10                                                           
         CLC   IOKEY(BWDKELSQ-BWDKEY),IOKEYSAV                                  
         BNE   VALP10                                                           
         IC    R9,BWDKELSQ                                                      
         B     VALP8                                                            
*                                                                               
VALP10   L     R3,AIOAREA2                                                      
         LA    R9,1(R9)                                                         
         STC   R9,BWDKELSQ                                                      
         STC   R9,BWDSEQ                                                        
         MVC   BWDKELST,BSTACD                                                  
         MVC   BWDSTACD,BSTACD                                                  
         GOTO1 AMIN,MINADD2        ADD RECORD BACK                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY(13),APRECKEY                                               
         B     VALP1                                                            
*                                                                               
VALP90   MVC   FVMSGNO,=AL2(FVFSET)                                             
         LA    R1,BWSKEYH                                                       
         ST    R1,FVADDR                                                        
         XC    BWSMSG,BWSMSG                                                    
         CLI   CHANGE,C'Y'                                                      
         BE    *+14                                                             
         MVC   BWSMSG(33),=C'** NO RECORDS NEED TO BE FIXED **'                 
         B     VALPX                                                            
         MVC   BWSMSG(19),=C'** RECORDS FIXED **'                               
*                                                                               
VALPX    B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
         EJECT                                                                  
LOCALD   DSECT                                                                  
*                                                                               
COMWRK   DS    0C                  COMMON BETWEEN BWS05 AND BWS98               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPNWS05WRK                                                     
         PRINT ON                                                               
         SPACE 2                                                                
LOCALD   DSECT                                                                  
         ORG   LOCALD+2048         BWS98 WORK AREA                              
*                                                                               
CHANGE   DS    CL1                                                              
*                                                                               
         ORG   LOCALD+4096                                                      
LOCALX   EQU   *                                                                
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
* SPNWSFBD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSFBD                                                       
         PRINT ON                                                               
         EJECT                                                                  
* SPNWSHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSHDR                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPNWS98A  06/25/92'                                      
         END                                                                    
