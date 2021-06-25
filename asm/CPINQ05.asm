*          DATA SET CPINQ05    AT LEVEL 002 AS OF 09/01/00                      
*PHASE TC0305A                                                                  
         TITLE 'COST PER POINT INQUIRY PROGRAM - DETAIL PHASE'                  
TC0305   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 4,**CPQD**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC              RC = GLOBAL W/S                              
         USING TC03TWA,RA          RA = TWA                                     
         SPACE 1                                                                
         CLI   NEXT,0              CONTINUATION SCREEN REQUESTED                
         BNE   D17                                                              
         EJECT                                                                  
*                  GENERATE TABLE TO DRIVE MAIN PROCESSING LOOP                 
         SPACE 3                                                                
D0       MVC   HEADING1,LINEONE                                                 
         LA    R9,MAINTAB                                                       
         USING MAINTABD,R9                                                      
         LA    R3,DTLTAB                                                        
         USING DTLTABD,R3                                                       
         SPACE 1                                                                
D1       MVC   MAINID,DTLID        ID                                           
         CLI   DTLID,X'FF'                                                      
         BE    D2                                                               
         MVC   HALF,DTLUPSR                                                     
         LH    R4,HALF                                                          
         AR    R4,RC                                                            
         MVC   MAINSRUP,0(R4)      ADDRESS OF MATRIX UPDATE S/R FOR             
         MVC   HALF,DTLDSSR        THIS DATA TYPE                               
         LH    R4,HALF                                                          
         AR    R4,RC                                                            
         MVC   MAINSRDS,0(R4)      ADDRESS OF VALUE DISPLAY S/R                 
         MVC   MAINSCRC(2),DTLSCRC START COLUMN NUMBER AND LENGTH               
         LA    R3,L'DTLTAB(R3)                                                  
         LA    R9,L'MAINTAB(R9)                                                 
         B     D1                                                               
         SPACE 1                                                                
LINEONE  DC    CL39'DPT-LN  POINTS    COST     CPP    IMPS '                    
         DC    CL39'    CPM   SPOTS     IPS     PPS     CPS'                    
         EJECT                                                                  
*                  MAIN PROCESSING LOOP TO UPDATE MATRIX                        
         SPACE 3                                                                
D2       LA    R6,SCANBLK                                                       
         USING MATRIXD,R6          R6 = A(MATRIX ENTRY BEING BUILT)             
         SPACE 1                                                                
D3       MVC   KEY,SAVEKEY                                                      
         SPACE 1                                                                
D4       GOTO1 AHIGH               READ A RECORD                                
         CLC   SAVEKEY(11),IO      CHECK KEY                                    
         BNE   D13                                                              
         LA    R4,IO                                                            
         USING CPKEYD,R4                                                        
         LA    R5,CPRECORD                                                      
         USING CPPERFD,R5                                                       
         GOTO1 AFILTER                                                          
         BZ    D12                                                              
         SR    R0,R0                                                            
         SPACE 1                                                                
D5       CLI   0(R5),0             LOOK FOR PERFORMANCE ELEMENT WITHIN          
         BE    D12                 DATE RANGE                                   
         CLC   ENDDAT,CPPYEAR                                                   
         BL    D12                                                              
         CLC   STARTDAT,CPPYEAR                                                 
         BH    D11                                                              
         LA    R9,MAINTAB                                                       
         SPACE 1                                                                
D10      CLI   0(R9),X'FF'         CREATE OR UPDATE A MATRIX ENTRY FOR          
         BE    D11                 EACH MAINTAB ENTRY                           
         LA    R6,SCANBLK                                                       
         MVI   FIRST,1                                                          
         MVC   MATDAYPT(2),CPKDAYPT                                             
         MVC   MATID,MAINID                                                     
         XC    MATVALA(8),MATVALA                                               
         GOTO1 ASEARCH                                                          
         BZ    D13                 MATRIX FULL                                  
         L     R6,DMCB             R6 = A(NEW OR OLD MATRIX ENTRY)              
         MVC   FULL,MAINSRUP                                                    
         L     RF,FULL                                                          
         GOTO1 (RF)                CALL MATRIX UPDATE S/R                       
         LA    R9,L'MAINTAB(R9)                                                 
         B     D10                                                              
         SPACE 1                                                                
D11      IC    R0,CPPLEN           ELEMENT BUMP                                 
         AR    R5,R0                                                            
         B     D5                                                               
         SPACE 1                                                                
D12      MVC   KEY,CPKEY           RECORD BUMP                                  
         L     R1,KEY+12                                                        
         AH    R1,=H'1'                                                         
         ST    R1,KEY+12                                                        
         B     D4                                                               
         SPACE 1                                                                
D13      LA    R3,L'MATRIX                                                      
         M     R2,RECNUM                                                        
         LA    R3,MATRIX(R3)                                                    
         MVI   0(R3),X'FF'                                                      
         ST    R3,AENDMTRX                                                      
         B     D14                                                              
         EJECT                                                                  
*                  SET UP DISPLAY SCREEN FROM MATRIX                            
         SPACE 3                                                                
D14      CLI   FIRST,0                                                          
         BNE   D20                                                              
         MVI   FERN,MINE                                                        
         MVC   CPQHEAD(L'NODSPLAY),NODSPLAY                                     
         B     DEND                                                             
NODSPLAY DC    CL50'NOTHING TO DISPLAY - ENTER FIELDS FOR NEXT INQUIRY'         
         SPACE 3                                                                
D17      GOTO1 ARDIR               IF CONTINUATION SCREEN, RESTORE              
         L     R1,ATIA             SAVED MATRIX VIA TIA                         
         USING TC03SAVX,R1                                                      
         LA    R2,SAVEMTRX                                                      
         LA    R4,L'MATRIX                                                      
         LH    R5,SAVEMXLN                                                      
         LA    R5,0(R2,R5)                                                      
         SR    R5,R4                                                            
         LA    R6,MATRIX-L'MATRIX                                               
         SPACE 1                                                                
D18      LA    R6,L'MATRIX(R6)                                                  
         MVC   0(L'MATRIX,R6),0(R2)                                             
         BXLE  R2,R4,D18                                                        
         ST    R6,AENDMTRX                                                      
         MVC   MAINTAB(241),SAVETAB                                             
         MVI   NEXT,0                                                           
         SPACE 3                                                                
D20      GOTO1 AFORMAT             SET UP DISPLAY                               
DEND     XMOD1 1                                                                
         EJECT                                                                  
*                  DETAIL REPORT INPUT TO MAINTAB                               
*                  COVERED BY DSECT DTLTABD                                     
         SPACE 3                                                                
DTLTAB   DS    0CL7 ID      UPDATE S/R   DISPLAY S/R   START COL,LENGTH         
         SPACE 1                                                                
         DC    AL1(01),AL2(APNTSUPD-GWS,APOINTDS-GWS),AL1(09,7) POINTS          
         DC    AL1(02),AL2(ACOSTUPD-GWS,AINTEGDS-GWS),AL1(17,7) COST            
         DC    AL1(03),AL2(ACPPUPD-GWS,ACPPDISP-GWS),AL1(27,5)  CPP             
         DC    AL1(04),AL2(AIMPSUPD-GWS,AINTEGDS-GWS),AL1(35,5) IMPS            
         DC    AL1(05),AL2(ACPMUPD-GWS,ACPMDISP-GWS),AL1(43,5)  CPM             
         DC    AL1(06),AL2(ASPTSUPD-GWS,AINTEGDS-GWS),AL1(51,5) SPOTS           
         DC    AL1(07),AL2(AIPSUPD-GWS,ANILDPDS-GWS),AL1(59,5)  IPS             
         DC    AL1(08),AL2(APPSUPD-GWS,APPSDISP-GWS),AL1(67,5)  PPS             
         DC    AL1(09),AL2(ACPSUPD-GWS,ANILDPDS-GWS),AL1(75,5)  CPS             
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                  NESTED INCLUDES FOR CPINQDSECT & CPGENFILE                   
         PRINT OFF                                                              
       ++INCLUDE CPINQDSECT                                                     
       ++INCLUDE CPGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CPINQ05   09/01/00'                                      
         END                                                                    
