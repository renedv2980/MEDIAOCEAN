*          DATA SET DEMSG95    AT LEVEL 020 AS OF 08/23/00                      
*PHASE DEMSG95A                                                                 
*********************************************************************           
* DEMSG95 - DECNVMSG: READS NTI-DAILIES MESSAGE FILE AND GENERATES              
*  A RECORD FOR THAT BOOK WEEK. RECORD GETS OVERWRITTEN EACH TIME               
*  PROGRAM IS RUN W/IN A GIVEN WEEK.  RECORD OUTPUT CONTAINS                    
*  KEY: PNNMSG1D..BK... WITH UP TO 20 80-BYTE ELEMS CONTAINING THE              
*  MESSAGE FILES LINES OF DATA.                                                 
*********************************************************************           
         TITLE 'GENERATE A MESSAGE FILE RECORD FROM NTI-MSG FILE'               
                                                                                
DEMSG95  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DEMSG95,RA,RR=RE                                               
         USING DEMCOND,R8          R8 POINTS TO GLOBAL WORKING STORAGE          
         SPACE 1                                                                
         ST    RE,RELO                                                          
         L     RC,ARREC                                                         
         LA    RC,4(RC)            RC POINTS TO RATING SERVICE RECORD           
         USING MIREC,RC                                                         
         SPACE 1                                                                
         L     R2,AIREC            R2 POINTS TO INTERIM RECORD - SORT           
         USING INTERD,R2           KEY, VALUES, AND DEMO VALUES                 
         SPACE 1                                                                
         B     *+4(R1)             ROUTINE HOOK                                 
         SPACE 1                                                                
         B     READ                PROCESS INPUT TAPE                           
         B     EXIT                SORT RECORD HOOK                             
         B     EXIT                E-O-F ON INPUT                               
         B     EXIT                                                             
         EJECT                                                                  
* *********************************************************************         
* READ - GET INPUT TAPE RECORDS ONE AT A TIME AND PROCESS. BUILD INTERM         
*        RECDS.                                                                 
* *********************************************************************         
READ     CLI   BYPREAD,X'FF'       DONE WITH TAPE?                              
         BE    REWND               YES, REWIND AND EXIT                         
         CLI   INTAPESW,1          1ST TIME IN TO READ TAPE?                    
         BE    GETLN               NO, GO GET A RECD FROM TAPE                  
         OPEN  (IN1,(INPUT))                                                    
         LA    R3,INTDATA          PT TO START OF STRG LOC                      
         ST    R3,NEXTLN           NEXT SLOT IN INTDATA FOR LINE                
         MVI   LINES,0                                                          
         MVI   INTAPESW,1                                                       
         MVI   BYPREAD,0           PROCESS                                      
*                                                                               
GETLN    DS    0H                                                               
         GET   IN1,(RC)            GET AN INPUT RECORD                          
         CLC   ASTERS(72),0(RC)    ENTIRE LINE OF ASTERISKS?                    
         BE    GETLN               YES, BYPASS IT AND READ NEXT RECD            
         CLC   SPCS,0(RC)          BLANK LINE?                                  
         BE    GETLN               YES, BYPASS THAT TOO                         
         MVC   0(80,R3),0(RC)      MOVE LINE INTO INTREC                        
         LA    R3,80(R3)                                                        
         ST    R3,NEXTLN                                                        
         MVI   0(R3),0             MAKE SURE SET TO NULL                        
         ZIC   R1,LINES            #LINES SAVED SO FAR                          
         LA    R1,1(R1)                                                         
         STC   R1,LINES                                                         
         CLI   LINES,20            MAX LINES=20                                 
         BL    GETLN                                                            
         B     DONE                                                             
*                                                                               
DONE     DS    0H                  BUILD KEY AND RELEASE RECORD                 
         LA    R7,INTKEY           BUILD PAV KEY                                
         USING PRKEY,R7                                                         
         MVC   PRCODE(8),=C'PNNMSG1D'                                           
         GOTO1 VDATCON,DMCB,(5,DATE),(0,DATE)  TODAY'S DATE->DATE               
*        PACK  DUB,DATE(2)                                                      
*        CVB   R0,DUB                                                           
*        STC   R0,BOOK                                                          
         GOTO1 VNETWEEK,DMCB,DATE,VGETDAY,VADDAY                                
         MVC   BOOK(2),4(R1)                                                    
         MVC   BOOK+1(1),8(R1)                                                  
         MVC   PRBOOK,BOOK                                                      
         MVC   INTBOOK,BOOK                                                     
         TIME  DEC                 GET SYSTEM TIME                              
         STCM  R0,12,INTPNO        SAVE TIME IN PRG# FIELD                      
         MVC   INTBTYP,LINES       TOTAL NUMBER LINES ON RECD                   
         MVI   BYPREAD,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
REWND    DS    0H                  FINISHED W/TAPE - REWIND AND EXIT            
         CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
*                                                                               
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* WORKING STORAGE                                                               
         DS    0F                                                               
RELO     DS    A                                                                
         DS    0D                                                               
SPCS     DC    CL80' '                                                          
ASTERS   DC    80C'*'                                                           
LINES    DC    X'00'               NUMBER LINES SAVED/PROCESSED SO FAR          
BYPREAD  DC    X'00'                                                            
BOOK     DS    CL2                                                              
DATE     DS    CL6                                                              
NEXTLN   DS    A                   STORES ADDR OF NEXT OPEN SLOT                
*                                                                               
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=80,                                               X        
               BLKSIZE=800,                                            X        
               MACRF=GM,                                               X        
               EODAD=DONE                                                       
         EJECT                                                                  
*        DENTHID                                                                
       ++INCLUDE DENTHID                                                        
         SPACE 1                                                                
         EJECT                                                                  
*        DEINTD                                                                 
       ++INCLUDE DEINTD                                                         
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
*        DEDEMFILE                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
*        DEDEMCNVD                                                              
       ++INCLUDE DEDEMCNVD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020DEMSG95   08/23/00'                                      
         END                                                                    
