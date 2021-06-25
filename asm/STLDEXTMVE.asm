*          DATA SET STLDEXTMVE AT LEVEL 018 AS OF 08/10/00                      
*PHASE STEXTMVA STEXTMVE                                                        
*INCLUDE BINSRCH2                                                               
*INCLUDE HEXOUT                                                                 
*INCLUDE CLPACK                                                                 
*        TITLE 'STLDEXTMVE - MOVE AGENCY TO AGENCY'                             
         TITLE 'STLDEXTMVE - MOVE AGENCY TO AGENCY'                             
***********************************************************************         
*                                                                     *         
*        THIS EXTRACT MOVES COMMERCIAL RECORDS FROM ONE AGENCY        *         
*        TO ANOTHER.                                                  *         
*                                                                     *         
*        THIS MODULE IS PART OF A SET TO COVER ALL SPOT RECORDS       *         
*                                                                     *         
*        SPLDEXTMVE - SPTDIR/FIL RECORDS                              *         
*        STLDEXTMVE - TRFDIR/FIL RECORDS                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
* PARAMETER LIST                                                      *         
*                                                                     *         
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                     *         
*                               X'01'= RECORD IN CORE                 *         
*                               X'FF'= END OF FILE                    *         
*               RETURN VALUE    X'00'= KEEP RECORD                    *         
*                               X'FF'= PURGE RECORD                   *         
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ        *         
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                     *         
*                               X'40'= TAPE OUTPUT                    *         
*                               X'20'= RECORD IS I/S FILE RECORD      *         
* P3=A(PARAM CARD)                                                    *         
* P4=A(FILE DEFN)                                                     *         
* P5=A(PRINTER)                                                       *         
* P6=A(CPRINT)                                                        *         
*                                                                     *         
***********************************************************************         
         TITLE 'STLDEXTMVE - MOVE AGENCY TO AGENCY - INIT'                      
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKLQ,STLDEXT                                                   
*                                                                               
         ST    R1,APARM            SAVE A(PARAMETER LIST)                       
         MVC   PLIST,0(R1)         SAVE PARAMETER LIST                          
*                                                                               
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
*                                                                               
         L     R9,VLDDEFN          ESTABLISH LOAD CONTROLS                      
         USING LDDEFND,R9                                                       
*                                                                               
         L     RF,=V(HEXOUT)                                                    
         ST    RF,VHEXOUT                                                       
*                                                                               
         L     RF,=V(CLPACK)                                                    
         ST    RF,VCLPACK                                                       
*                                                                               
         B     INITX                                                            
*                                                                               
*        DATA FOR THIS CONVERSION                                               
*                                                                               
MVAGYOLD DC    C'TH'               OLD AGENCY - ZENITH                          
MVAGOLD  DC    X'90'               OLD LOCAL AGENCY - ZENITH                    
*                                                                               
MVCLTOLD DC    C'OND'              OLD CLIENT                                   
MVCLPOLD DS    XL2'00'             OLD CLIENT - PACKED                          
*                                                                               
MVAGYNEW DC    C'OM'               NEW AGENCY - O&M                             
MVAGNEW  DC    X'10'               NEW LOCAL AGENCY - O&M                       
*                                                                               
MVCLTNEW DC    C'XXX'              NEW CLIENT                                   
MVCLPNEW DS    XL2'00'             NEW CLIENT PACKED                            
*                                                                               
*        TABLE OF WHAT TO MOVE                                                  
*                                                                               
MVTAB    DS    0X                  TABLE ENTRY                                  
MVTAGYMD DC    X'91'               AGENCY/MEDIA                                 
MVTEST   DC    AL1(13)             ESTIMATE                                     
MVTABL   EQU   *-MVTAB             TABLE ENTRY LENGTH                           
*                                                                               
         DC    X'91',AL1(14)                                                    
         DC    X'91',AL1(15)                                                    
         DC    X'91',AL1(16)                                                    
         DC    X'92',AL1(17)                                                    
         DC    X'92',AL1(18)                                                    
*                                                                               
         DC    X'FF'               EOT                                          
*                                                                               
INITX    DS    0H                                                               
*                                                                               
         TITLE 'STLDEXTMVE - MOVE AGENCY TO AGENCY - DMCTL'                     
***********************************************************************         
*                                                                     *         
*        CONTROL FLOW LOGIC                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXCTL   DS    0H                                                               
*                                                                               
         CLI   PRMMODE,PRMMINIQ                                                 
         BE    DMXINIT             INITIALIZE                                   
*                                                                               
         CLI   PRMMODE,PRMMRECQ    NEW RECORD IN CORE                           
         BE    DMXREC              PROCESS                                      
*                                                                               
         CLI   PRMMODE,PRMMEOFQ                                                 
         BE    DMXEOF              END-OF-FILE                                  
*                                                                               
         B     DMXIT                                                            
*                                                                               
*        EXITS                                                                  
*                                                                               
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
*                                                                               
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),PRMRKPQ                                                    
         B     DMXIT                                                            
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),PRMRPRGQ                                                   
         B     DMXIT                                                            
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),PRMREOJQ                                                   
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXIT    XMOD1 1                                                                
*                                                                               
         TITLE 'STLDEXTMVE - MOVE AGENCY TO AGENCY - DMXINIT'                   
***********************************************************************         
*                                                                     *         
*        PROGRAM INITIALIZATION                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXINIT  DS    0H                                                               
*                                                                               
*        PACK OLD AND NEW CLIENTS                                               
*                                                                               
         GOTO1 VCLPACK,DMCB,MVCLTOLD,MVCLPOLD                                   
*                                                                               
         GOTO1 VCLPACK,DMCB,MVCLTNEW,MVCLPNEW                                   
*                                                                               
*        PRINT TITLES                                                           
*                                                                               
         ZAP   LINE,=P'100'        FORCE NEW PAGE                               
*                                                                               
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
*                                                                               
         MVC   MID1+2(8),=CL8'OLD KEY'                                          
         MVC   MID1+32(8),=CL8'NEW KEY'                                         
         MVC   MID1+65(3),=C'OLD'                                               
         MVC   MID1+72(3),=C'NEW'                                               
*                                                                               
         OPEN  (FILEOUT,(OUTPUT))    OPEN OUTPUT FILE                           
*                                                                               
DMINITX  DS    0H                                                               
         B     DMXIT                                                            
*                                                                               
         TITLE 'STLDEXTMVE - MOVE AGENCY TO AGENCY - DMXREC'                    
***********************************************************************         
*                                                                     *         
*        PROCESS NEXT RECORD TO BE ADDED TO FILE                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXREC   DS    0H                                                               
*                                                                               
         SR    R6,R6               INIT ELEMENT POINTER                         
*                                                                               
         L     R3,AREC             POINT TO RECORD FOR PROCESSING               
*                                                                               
*        DETERMINE RECORD TYPE                                                  
*                                                                               
         CLC   =X'0A21',0(R3)      TEST TRAFFIC COMMERCIAL PROFILE              
         BE    CML                                                              
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
         TITLE 'STLDEXTMVE - MOVE AGENCY TO AGENCY - INS'                       
***********************************************************************         
*                                                                     *         
*        TRAFFIC COMMERCIAL PROFILE RECORD - X'0A21'                  *         
*                                                                     *         
*NTRY    R3==> RECORD                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CML      DS    0H                                                               
*                                                                               
         USING CMLRECD,R3          ESTABLISH TRAFFIC COMMERCIAL PROFILE         
*                                                                               
         LA    R4,MVTAB            POINT TO TABLE OF ITEMS TO BE MOVED          
*                                                                               
CMLKEYLP DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         DONE AT END OF TABLE                         
         BE    CMLKEYDN                                                         
*                                                                               
         CLC   CMLKAM,MVTAGYMD-MVTAB(R4)  MATCH ON AGENCY/MEDIA                 
         BNE   CMLKEYCN                                                         
*                                                                               
         CLC   CMLKCLT,MVCLPOLD            MATCH ON PACKED CLIENT               
         BNE   CMLKEYCN                                                         
*                                                                               
         B     CMLKEYFD            ACCEPT CML                                   
*                                                                               
CMLKEYCN DS    0H                                                               
*                                                                               
         LA    R4,MVTABL(R4)       POINT TO NEXT ENTRY IN TABLE                 
         B     CMLKEYLP                                                         
*                                                                               
CMLKEYFD DS    0H                                                               
*                                                                               
         BRAS  RE,COPY             COPY RECORD TO WORKAREA                      
*                                                                               
         LA    R3,NEWREC           POINT TO NEW RECORD                          
         USING CMLRECD,R3          ESTABLISH SPOT CML RECORD                    
*                                                                               
         NI    CMLKAM,X'0F'        KILL OLD AGENCY NYBBLE                       
         OC    CMLKAM,MVAGNEW      ADD IN NEW AGENCY NYBBLE                     
*                                                                               
         MVC   CMLKCLT,MVCLPNEW    REPLACE CLIENT CODE                          
*                                                                               
         BRAS  RE,WRITE            WRITE RECORD TO OUTPUT DATASET               
*                                                                               
CMLKEYDN DS    0H                                                               
*                                                                               
CMLX     DS    0H                                                               
         B     DMXKEEP                                                          
*                                                                               
         TITLE 'STLDEXTMVE - MOVE BETWEEN AGENCIES - COPYREC'                   
***********************************************************************         
*                                                                     *         
*        COPY CURRENT RECORD TO WORKAREA                              *         
*                                                                     *         
*NTRY    R3==> RECORD TO BE COPIED                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
COPY     NTR1  LABEL=*                                                          
*                                                                               
         LR    R0,R3               POINT TO INCOMING RECORD                     
         LA    RE,NEWREC           POINT TO STORAGE AREA                        
         SR    R1,R1                                                            
         ICM   R1,3,13(R3)         GET RECORD LENGTH                            
         LR    RF,R1               COPY LENGTH                                  
*                                                                               
         LA    RF,4(RF)            ALLOW FOR LENGTH BYTES                       
         STCM  RF,3,RRLEN          SET OUTPUT FILE LENGTH                       
         AHI   RF,-4               RESTORE TRUE RECORD LENGTH                   
*                                                                               
         MVCL  RE,R0               COPY INCOMING RECORD                         
*                                                                               
         CLC   MVAGYOLD,NEWREC+20  IF ALPHA AGENCY PRESENT                      
         BNE   *+10                                                             
         MVC   NEWREC+20(2),MVAGYNEW                                            
*                                                                               
COPYX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'STLDEXTMVE - MOVE BETWEEN AGENCIES - WRITE'                     
***********************************************************************         
*                                                                     *         
*        WRITE WORKAREA RECORD TO OUTPUT DATASET                      *         
*                                                                     *         
*NTRY    NEWREC HAS RECORD TO GO                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WRITE    NTR1  LABEL=*                                                          
*                                                                               
         L     R6,AREC             POINT TO START OF RECORD                     
         MVI   ELCODE,X'20'        FIND PRODUCT ELEMENT                         
         BRAS  RE,GETEL                                                         
         BNZ   WRIELMX             NONE FOUND                                   
*                                                                               
         CLI   1(R6),3             PRINT IF MORE THAN 2 PRODUCTS                
         BH    WRIPRNT                                                          
*                                                                               
WRIELMX  DS    0H                                                               
*                                                                               
         SP    CTR,=P'1'           DECREMENT COUNTER                            
         BNZ   WRIPRNTX            PRINT EVERY 10TH RECORD                      
*                                                                               
         ZAP   CTR,=P'10'                                                       
*                                                                               
WRIPRNT  DS    0H                                                               
*                                                                               
         L     R5,AREC                                                          
         GOTO1 VHEXOUT,DMCB,(R5),P+2,13,0,0  PRINT OLD KEY                      
*                                                                               
         GOTO1 VHEXOUT,DMCB,NEWREC,P+32,13,0,0  PRINT NEW KEY                   
*                                                                               
         CLI   0(R6),0             IF PRODUCT ELM FOUND                         
         BE    WRIPRNT9                                                         
*                                                                               
         CLI   1(R6),3             AND MORE THAN 2 PRODUCTS                     
         BNH   WRIPRNT9                                                         
*                                     PRINT ELEMENT                             
         GOTO1 VHEXOUT,DMCB,(R6),P+70,4,0,0                                     
*                                                                               
WRIPRNT9 DS    0H                                                               
*                                                                               
         GOTO1 VPRINTER            PRINT TRACE                                  
*                                                                               
WRIPRNTX DS    0H                                                               
*                                                                               
         PUT   FILEOUT,RRLEN       WRITE TO OUTPUT FILE                         
*                                                                               
WRITEX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         GETEL R6,42,ELCODE                                                     
*                                                                               
FILEOUT  DCB   DDNAME=TEMPOUT,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               BLKSIZE=25000                                                    
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'STLDEXTMVE - MOVE AGENCY TO AGENCY - WORKD'                     
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WORKD    DS    0D                                                               
WORK     DS    CL128                                                            
DMCB     DS    6F                                                               
APARM    DS    A                   A(PARAMETER LIST)                            
*                                                                               
BYTE     DS    X                                                                
SWITCH   DC    X'00'               C'B' - INDICATES MKT FROM BUY RECORD         
COUNT    DS    F                                                                
CTR      DC    PL2'10'             TRACE COUNTER                                
*                                                                               
TABADDR  DS    A                   A(BINSRCH TABLE)                             
DUB      DS    D                                                                
KEY      DS    CL10                                                             
BAGYMD   DS    CL1                                                              
WAGYMD   DS    CL1                 MASTER AGY/MEDIA                             
PRINTSW  DC    XL1'00'             X'01' - PRINT TRACE                          
OLDKEY   DC    XL13'00'            OLD KEY SAVEAREA                             
NEWKEY   DC    XL13'00'            NEW KEY SAVEAREA                             
*                                                                               
*        PARAMETER LIST SAVE AREA                                               
*                                                                               
         DS    0D                  ALIGNMENT                                    
PLIST    DS    0CL24               PARAMETER LIST - SAVED                       
PRMMODE  DS    0XL1                CALLING MODE                                 
PRMMINIQ EQU   X'00'                 X'00'= INITIALISE                          
PRMMRECQ EQU   X'01'                 X'01'= RECORD IN CORE                      
PRMMEOFQ EQU   X'FF'                 X'FF'= END OF FILE                         
*                                                                               
PRMRTNCD DS    0XL1                RETURN CODE                                  
PRMRKPQ  EQU   X'00'               X'00'= KEEP RECORD                           
PRMRPRGQ EQU   X'FF'               X'FF'= PURGE RECORD                          
PRMREOJQ EQU   X'FF'               X'FF'/C'EOJ'=PURGE & CAUSE EOJ               
*                                                                               
AREC     DS    A                   A(CURRENT RECORD)                            
*                                                                               
VTAPEOUT DS    A                   V(TAPEOUT DCB)                               
APARAMC  DS    A                   A(PARAMETER CARD)                            
VLDDEFN  DS    A                   A(FILE DEFINITION)                           
VPRINTER DS    A                   V(PRINTER)                                   
VCPRINT  DS    A                   V(CPRINT)                                    
VHEXOUT  DS    A                   V(HEXOUT)                                    
VCLPACK  DS    A                   V(CLPACK)                                    
*                                                                               
ELCODE   DS    X                                                                
*                                                                               
         DS    0A                  ALIGNMENT                                    
       ++INCLUDE DDBSRPRMD                                                      
*                                                                               
RRLEN    DS    XL2                 OUTPUT RECORD LENGTH                         
         DS    XL2                 SPARE                                        
NEWREC   DS    XL4096              NEW RECORD BUILD AREA                        
*                                                                               
WORKLQ   EQU   *-WORKD             LENGTH OF WORKING STORAGE                    
*                                                                               
         EJECT                                                                  
*DMLDDEFN                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
*DDDPRINT                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
*SPTRCMML                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPTRCMML                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018STLDEXTMVE08/10/00'                                      
         END                                                                    
