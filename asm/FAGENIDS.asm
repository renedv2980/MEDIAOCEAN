*          DATA SET FAGENIDS   AT LEVEL 003 AS OF 09/05/03                      
*PHASE T00AFCA                                                                  
*INCLUDE GETIDS                                                                 
         TITLE 'FAGENIDS - BUILD TABLE OF USER-IDS FOR GENERIC IDS'             
GENIDS   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**GENIDS,RR=RE,CLEAR=YES                             
         USING WORKD,RC                                                         
         ST    RE,RELO                                                          
         LA    RE,*+6                                                           
         BSM   0,RE                SET IN 24-BIT MODE                           
*                                                                               
         L     RE,=V(GETIDS)                                                    
         A     RE,RELO                                                          
         ST    RE,VGETIDS                                                       
         LR    RA,R1               RA=A(PARAMETER LIST)                         
         L     R1,0(RA)            P1=A(GENERIC USER-ID NUMBER)                 
         MVC   GENID,0(R1)         SAVE GENERIC USER-ID                         
         MVC   VDATAMGR,4(RA)      P2=A(DATAMGR)                                
         XC    0(8,RA),0(RA)       CLEAR OUTPUT PARAMETERS                      
         CLC   GENLOCK,LOCKWORD    IS GENERIC ID TABLE LOCKED                   
         BNE   GENIDS2                                                          
*                                  DO I/O LOOP UNTIL TABLE IS UNLOCKED          
         XC    IO(25),IO                                                        
GENIDS1  GOTO1 VDATAMGR,DMCB,(0,DMRDHI),CTFILE,IO,IO                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   GENLOCK,LOCKWORD    TEST TABLE UNLOCKED                          
         BNE   GENIDS2                                                          
         ZIC   R1,IO               NO - BUMP FIRST BYTE OF KEY                  
         LA    R1,1(R1)                                                         
         STC   R1,IO                                                            
         B     GENIDS1                                                          
         EJECT                                                                  
* BUILD/RETURN LIST OF USER-IDS ATTACHED TO A GENERIC USER-ID                   
*                                                                               
* NTRY - R1=A(GENERIC ID NUMBER)                                                
*                                                                               
* EXIT - CC=EQ IF OK     - P1=N'ENTRIES IN LIST                                 
*                        - P2=A(LIST OF USER-IDS)                               
*        CC=NEQ ON ERROR - P1/P2 SET TO BINARY ZEROES                           
*                                                                               
* NOTE - GENIDTBL IS A TABLE USED TO OPTIMISE LOOK-UPS OF GENERIC IDS.          
*        THE STRUCTURE OF THE TABLE IS:-                                        
*                                                                               
*        TABLE HEADER    - H'TABLE LENGTH'                                      
*                          H'SPACE USED SO FAR'                                 
*                                                                               
*        TABLE ENTRIES   - H'GENERIC USER-ID'      (X'80' BIT ON)               
*                          H'NUMBER OF USER-IDS'                                
*                          H'USER-ID 1'            (DESTINATION ID)             
*                          H'.........'            (DESTINATION ID)             
*                          H'USER-ID N'            (DESTINATION ID)             
*                                                                               
*        TABLE TRAILER   - H'0'                                                 
*                                                                               
GENIDS2  MVC   GENLOCK,LOCKWORD    LOCK-OUT TABLE                               
         MVI   FLAG,0              SET NO ERRORS                                
         LA    R5,GENIDTBL         R5=A(TABLE HEADER)                           
         LA    R6,4(R5)            R6=A(FIRST ENTRY)                            
*                                                                               
GENIDS4  OC    0(2,R6),0(R6)       TEST E-O-T                                   
         BZ    GENIDS6                                                          
         CLC   0(2,R6),GENID       NO - MATCH GENERIC ID TO TABLE               
         BE    GENIDS8                                                          
         LH    RE,2(R6)            RE=N'IDS FOR THIS GENERIC ID                 
         SLL   RE,1                                                             
         LA    R6,4(RE,R6)         BUMP TO NEXT TABLE ENTRY                     
         B     GENIDS4                                                          
*                                                                               
GENIDS6  LA    R1,IO               READ GENERIC ID & BUILD ID LIST              
         USING CTIREC,R1                                                        
         XC    CTIKEY,CTIKEY       BUILD KEY OF GENERIC-ID RECORD               
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),GENID                                                
         NI    CTIKID+8,255-X'80'  TURN OFF GENERIC ID FLAG                     
         GOTO1 VDATAMGR,DMCB,(0,DMREAD),CTFILE,IO,IO                            
         CLI   8(R1),0             TEST DATAMGR ERRORS                          
         BE    *+12                                                             
         MVI   FLAG,2                                                           
         B     GENIDSX                                                          
*                                  BUILD LIST OF DESTINATION IDS                
         GOTO1 VGETIDS,DMCB,(C'D',IO),0,VDATAMGR                                
         CLI   0(R1),X'FF'         TEST GETIDS ERRORS                           
         BNE   *+12                                                             
         MVI   FLAG,3              YES - EXIT WITH ERROR                        
         B     GENIDSX                                                          
         CLI   0(R1),0             TEST ANY IDS IN LIST                         
         BNE   *+12                                                             
         MVI   FLAG,4              NO - EXIT WITH ERROR                         
         B     GENIDSX                                                          
         ZIC   RE,0(R1)            RE=N'IDS IN LIST                             
         L     RF,4(R1)            RF=A(DESTINATION ID LIST)                    
         LR    R1,RE                                                            
         SLL   R1,1                                                             
         LA    R1,6(R6,R1)         R1=A(NEW END OF TABLE)                       
         SR    R1,R5               R1=NEW LENGTH OF TABLE                       
         CH    R1,0(R5)            TEST ENOUGH SPACE FOR TABLE ENTRY            
         BNH   *+6                                                              
         DC    H'0'                NO - DIE                                     
         STH   R1,2(R5)            SET NEW L'TABLE                              
         MVC   0(2,R6),GENID       SET GENERIC ID NUMBER                        
         STH   RE,2(R6)            SET N'ENTRIES IN LIST                        
         LA    R1,4(R6)            BUILD USER-ID LIST                           
         MVC   0(2,R1),10(RF)                                                   
         LA    R1,2(R1)                                                         
         LA    RF,12(RF)                                                        
         BCT   RE,*-14                                                          
         XC    0(2,R1),0(R1)       CLEAR E-O-T                                  
*                                                                               
GENIDS8  LH    RE,2(R6)                                                         
         ST    RE,0(RA)            SET N'ENTRIES IN LIST                        
         LA    R6,4(R6)                                                         
         ST    R6,4(RA)            SET A(ID LIST)                               
*                                                                               
GENIDSX  XC    GENLOCK,GENLOCK     CLEAR TABLE LOCKWORD                         
         CLI   FLAG,0              SET RETURN CONDITION CODE                    
         XMOD1 1                                                                
         ORG   *-2                                                              
         BSM   0,RE                RESTORE CALLER'S ADDRESSING MODE             
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
DMREAD   DC    C'DMREAD  '                                                      
DMRDHI   DC    C'DMRDHI  '                                                      
CTFILE   DC    C'CTFILE  '                                                      
LOCKWORD DC    C'**LOCK**'                                                      
         SPACE 1                                                                
         DS    0H                                                               
GENLOCK  DC    XL8'00'             CONTAINS LOCKWORD IF TABLE IN USE            
GENIDLEN EQU   8192                MAXIMUM LENGTH OF GENIDTBL                   
GENIDTBL DC    AL2(GENIDLEN,0)                                                  
         DC    (GENIDLEN)X'00'                                                  
         SPACE 1                                                                
* DSECT TO COVER TEMP W/S                                                       
*                                                                               
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
RELO     DS    A                                                                
VDATAMGR DS    V                                                                
VGETIDS  DS    V                                                                
GENID    DS    H                   GENERIC USER-ID                              
FLAG     DS    X                                                                
IO       DS    1000C               I/O AREA FOR CTFILE READS                    
WORKX    EQU   *                                                                
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003FAGENIDS  09/05/03'                                      
         END                                                                    
