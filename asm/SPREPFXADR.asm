*          DATA SET SPREPFXADR AT LEVEL 008 AS OF 03/17/93                      
*PHASE SPFX02WH                                                                 
SPFX02WH TITLE 'SPREPFXADR - GET RID OF ALL BAD CABLE ADDRESS RECORDS'          
SPFX02WH CSECT                                                                  
         DS    8192C                                                            
         ORG   *-8192                                                           
         PRINT NOGEN                                                            
*                                                                               
         NMOD1 0,SPFX02,R7                                                      
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02WH+4096,RC                                                 
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    MAIN                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* MAIN                                                                          
***************                                                                 
*     MAIN SECTION OF THE PROGRAM                                               
***********************************************************************         
*                                                                               
MAIN     DS    0H                                                               
         LR    R6,RB               SET ADDRESS OF RECORD FOR SPONSOR            
         A     R6,=A(SPOTREC-SPFX02WH)                                          
         ST    R6,AREC                                                          
         ST    R6,ADSTAT                                                        
         ST    R6,ADSTATAD                                                      
*                                                                               
         XC    NUMGOOD,NUMGOOD                                                  
         XC    NUMBADD,NUMBADD                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(7),=CL7'AT0001T'                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),STATION,KEY,AREC                     
         CLI   DMCB+8,X'02'                                                     
         BE    MAIN10                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MAIN10   L     R6,AREC                                                          
         USING GENADDD,R6                                                       
         CLI   ADDKTYPE,ADDKTYPQ                                                
         BNE   MAIN30                                                           
         CLI   ADDKMED,C'T'                                                     
         BNE   MAIN30                                                           
*                                                                               
         CLC   ADDKFILL,=6C'0'     BAD ADDRESS RECORD?                          
         BE    MAIN20              NO, CHECK NEXT RECORD                        
         MVC   P(L'ADDRKEY),ADDRKEY                                             
         MVC   P+20(25),=CL25'IS MARKED FOR DELETION!!!!'                       
         GOTO1 REPORT                                                           
         L     R1,NUMBADD                                                       
         AH    R1,=H'1'                                                         
         ST    R1,NUMBADD                                                       
         B     MAINNEXT                                                         
*                                                                               
MAIN20   NI    ACNTL,X'FF'-X'80'         CHANGE TO DELETED                      
         XC    KEY,KEY                                                          
         MVC   KEY(L'ADDRKEY),ADDRKEY                                           
         GOTO1 DATAMGR,DMCB,DMWRT,STATION,KEY,AREC                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P(L'ADDRKEY),ADDRKEY                                             
         MVC   P+20(12),=CL12'IS OKAY!!!!!'                                     
         GOTO1 REPORT                                                           
         L     R1,NUMGOOD                                                       
         AH    R1,=H'1'                                                         
         ST    R1,NUMGOOD                                                       
*                                                                               
MAINNEXT GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),STATION,KEY,AREC                     
         CLI   DMCB+8,X'02'                                                     
         BE    MAIN10                                                           
         CLI   DMCB+8,0                                                         
         BE    MAIN10                                                           
         DC    H'0'                                                             
*                                                                               
MAIN30   MVC   P(33),=CL33'NUMBER OF GOOD ADDRESS RECORDS:  '                   
         EDIT  (B4,NUMGOOD),(5,P+34),ALIGN=LEFT,ZERO=YES                        
         GOTO1 REPORT                                                           
         MVC   P(33),=CL33'NUMBER OF BAD ADDRESS RECORDS:   '                   
         EDIT  (B4,NUMBADD),(5,P+34),ALIGN=LEFT,ZERO=YES                        
         GOTO1 REPORT                                                           
         L     R2,NUMGOOD                                                       
         A     R2,NUMBADD                                                       
         MVC   P(33),=CL33'TOTAL NUMBER OF ADDRESS RECORDS: '                   
         EDIT  (R2),(5,P+34),ALIGN=LEFT,ZERO=YES                                
         GOTO1 REPORT                                                           
*                                                                               
MAINX    B     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
NUMGOOD  DS    F                   COUNTER OF GOOD ADDRESS RECORDS              
NUMBADD  DS    F                   COUNTER OF BAD ADDRESS RECORDS               
*                                                                               
SPOTREC  DS    CL4000                                                           
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
GENADDD  DSECT                                                                  
       ++INCLUDE SPGENADD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SPREPFXADR03/17/93'                                      
         END                                                                    
