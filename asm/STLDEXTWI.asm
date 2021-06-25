*          DATA SET STLDEXTWI  AT LEVEL 029 AS OF 10/15/96                      
*PHASE STLDWI                                                                   
*INCLUDE HEXOUT                                                                 
         TITLE 'DMLDEXTWI - MOVE TWX NUMBERS FOR WESTERN'                       
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 20,DMLDEXT                                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         L     RF,=V(HEXOUT)                                                    
         ST    RF,VHEXOUT                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         SPACE 2                                                                
*                                                                               
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         MVC   P(15),=C'STLDWI BE HERE'                                         
         GOTO1 VPRINTER                                                         
         OPEN  (FILEIN,INPUT)                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     DMXIT                                                            
*                                                                               
DMXREC   DS    0H                                                               
         TM    MYFLAG,DONE         DONE GETTING FILEIN?                         
         BO    DMXKEEP             YES                                          
*                                                                               
         L     R7,AREC                                                          
         USING ADDRECD,R7                                                       
*                                                                               
         CLI   ADDKTYPE,C'A'       STATION RECORD?                              
         BNE   DMXKEEP             NO                                           
*                                                                               
         CLI   ADDKMED,C'R'        RADIO?                                       
         BE    *+12                                                             
         CLI   ADDKMED,C'T'        TELEVISION?                                  
         BNE   DMXKEEP                                                          
*                                                                               
         TM    MYFLAG,CALLKEEP     CALL DMXKEEP?                                
         BO    DMXREC7             YES                                          
*                                                                               
DMXREC5  GET   FILEIN,REC                                                       
*                                                                               
DMXREC7  LA    R3,REC                                                           
         USING FILEIND,R3                                                       
*                                                                               
         CLC   ADDRREC+1(8),REC    SAME MEDIA?                                  
         BL    DMXKEEPF            ADDKMED < MEDFIN                             
         BH    DMXREC5             MEDFIN < ADDKMED                             
         B     DMXREC10                                                         
*                                                                               
DMXKEEPF OI    MYFLAG,CALLKEEP     CALLED DMXKEEP                               
         B     DMXKEEP                                                          
*                                                                               
DMXREC10 NI    MYFLAG,X'FF'-CALLKEEP                                            
         MVC   APHONE,TWIXFIN                                                   
         L     R1,COUNT            COUNTER                                      
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
         C     R1,=F'261'                                                       
         BH    DMXKEEP                                                          
         MVC   P(ADDKEYLQ-L'ADDKFILL),ADDRREC                                   
         MVC   P+ADDKEYLQ(L'APHONE),APHONE                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
DMXEND   DS    0H                                                               
         MVC   P(13),=C'GOT TO DMXEND'                                          
         GOTO1 VPRINTER                                                         
         CLOSE FILEIN                                                           
         OI    MYFLAG,DONE         DONE GETTING FILEIN                          
         B     DMXKEEP                                                          
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(13),=C'GOT TO DMXEOF'                                          
         GOTO1 VPRINTER                                                         
         TM    MYFLAG,DONE         DONE GETTING FILEIN?                         
         BO    DMXIT               YES                                          
         CLOSE FILEIN                                                           
         B     DMXIT                                                            
         EJECT                                                                  
                                                                                
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,MACRF=GM,RECFM=FB,LRECL=28,      X        
               EODAD=DMXEND                                                     
*                                                                               
BYTE     DS    X                                                                
COUNT    DS    F                                                                
ACOUNT   DS    F                                                                
MCOUNT   DS    F                                                                
SCOUNT   DS    F                                                                
ELCODE   DS    X                                                                
MYFLAG   DS    X                   FLAGS                                        
REC      DS    CL28                RECORD FROM FILEIN                           
CALLKEEP EQU   X'01'               CALLED DMXKEEP                               
DONE     EQU   X'02'               DONE GETTING FILEIN                          
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
VHEXOUT  DS    A                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
         EJECT                                                                  
         PRINT OFF                                                              
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
ADDRECD  DSECT                                                                  
       ++INCLUDE SPGENADD                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
FILEIND  DSECT                     FILEIN DSECT                                 
MEDFIN   DS    CL1                 MEDIA                                        
CALLFIN  DS    CL5                 CALL LETERS                                  
AGYFIN   DS    CL2                 AGENCY CODE                                  
TWIXFIN  DS    CL20                TWIX NUMBER                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029STLDEXTWI 10/15/96'                                      
         END                                                                    
