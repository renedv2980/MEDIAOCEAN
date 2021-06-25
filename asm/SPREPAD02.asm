*          DATA SET SPREPAD02  AT LEVEL 016 AS OF 05/01/02                      
*PHASE SPAD02A                                                                  
         TITLE 'SPAD02 - STATION ADDRESS COMMERCIAL TYPE CHANGE'                
SPAD02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPAD02                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    BUILDTP                                                          
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
* REQFRST                                                                       
*                                                                               
BUILDTP  DS    0H                                                               
*                                                                               
         OPEN  (FILEOUT,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A28'                                                  
         MVC   KEY+2(1),SVAGYMD                                                 
         GOTO1 HIGH                                                             
         B     BLD15                                                            
*                                                                               
BLD10    DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
BLD15    DS    0H                                                               
         XC    P1,P1                                                            
         MVI   LONGLEN,0                                                        
*                                                                               
BLD20    CLC   KEY(3),KEYSAVE          TEST STATION ADDRESS RECS                
         BNE   BLDEND                                                           
*                                                                               
         TM    KEY+13,X'80'                                                     
         BO    BLD10                                                            
*                                                                               
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         USING STARECD,R6                                                       
*                                                                               
         LA    R5,RECWORK                                                       
         USING NEWREC,R5                                                        
*                                                                               
         MVC   NEWMED(L'NEWMED),STAKAM                                          
         MVC   NEWSTA(L'NEWSTA),STAKSTA                                         
*                                                                               
         L     R2,ADBUY                                                         
         USING STADTAEL,R2                                                      
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   BLD10               GET NEXT RECORD                              
         B     *+14                                                             
*                                                                               
BLD50    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   BLD10                                                            
         DC    H'0'                DIE IF HAS NEXT ELEMENT                      
*                                                                               
         ZIC   R1,STADTALN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NEWELEM(0),0(R2)       MOVE INTO NEW RECORD                      
         CLI   STADTALN,151        TEST ALREADY LONG ELEMENT                    
         BNE   *+12                NO, THEN CONTINUE                            
         MVI   LONGLEN,C'Y'                                                     
         B     BLD70                                                            
*                                                                               
         MVI   NEWELEM+1,X'97'      ADJUST LENGTH IN NEW REC                    
         MVC   NEWEND(L'NEWEND),=XL32'00'                                       
*                                                                               
BLD70    DS    0H                                                               
         LA    R3,P1                                                            
         MVC   0(L'NEWMED,R3),NEWMED                                            
         MVC   1(L'NEWSTA,R3),NEWSTA                                            
         MVC   10(5,R3),NEWELEM                                                 
         MVC   20(L'NEWEND,R3),NEWEND                                           
         CLI   LONGLEN,C'Y'                                                     
         BNE   *+10                                                             
         MVC   55(3,R3),=X'FFFFFF'                                              
*                                                                               
         XR    R0,R0                                                            
         LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT                                                         
         GOTO1 PRNTBL,DMCB,=C'NEWREC',(R3),C'DUMP',60,=C'1D'                    
         DROP  R5,R6,R2                                                         
         EJECT                                                                  
*                                                                               
*                                                                               
BLD999   DS    0H                                                               
         BAS   RE,OUTFILE                                                       
         B     BLD50                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*              ROUTINE TO MOVE RECORD TO OUTFILE                                
*                                                                               
*                                                                               
OUTFILE  NTR1                                                                   
*                                                                               
*                                                                               
OUT200   DS    0H                                                               
*                                                                               
OUT250   DS    0H                                                               
         LA    R0,RECWORK          GET A(RECORD)                                
         LA    R1,FILEOUT                                                       
         PUT   (R1),(R0)                                                        
*                                                                               
OUTX     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
BLDEND   DS    0H                                                               
         CLOSE (FILEOUT)                                                        
         XR    RE,RE                                                            
         LH    RE,COUNT                                                         
         EDIT  (RE),(4,P+5),ALIGN=LEFT                                          
         MVC   P+10(18),=C'RECORDS ON OUTFILE'                                  
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         SPACE 3                                                                
         GETEL R2,24,ELCODE                                                     
         SPACE 3                                                                
ELCODE   DS    CL1                                                              
COUNT    DS    H                                                                
VSORTER  DC    V(SORTER)                                                        
LONGLEN  DS    CL1                                                              
RECWORK  DS    CL157               RECORD WORK AREA                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         SPACE 4                                                                
*                                                                               
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=FB,MACRF=PM,              X        
               LRECL=157,BUFNO=2,BLKSIZE=15700                                  
         SPACE 4                                                                
NEWREC   DSECT                     RECORD BUILT ON OUTPUT TAPE                  
         ORG                                                                    
NEWENT   DS    0XL157                                                           
NEWMED   DS    XL1                 MEDIA                                        
NEWSTA   DS    CL5                 STATION                                      
NEWELEM  DS    0XL151              ELEMENT                                      
OLDELEM  DS    XL119               OLD SHORTER ELEM                             
NEWEND   DS    0CL32                                                            
         DS    CL28                                                             
NEWCMLT  DS    CL4                 COMMERCIAL TYPE FOR STATION                  
*                                                                               
         SPACE 6                                                                
*                                                                               
       ++INCLUDE SPTRSTA                                                        
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SPREPAD02 05/01/02'                                      
         END                                                                    
