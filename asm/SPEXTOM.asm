*          DATA SET SPEXTOM    AT LEVEL 008 AS OF 09/16/97                      
*          DATA SET SPEXTBJ    AT LEVEL 011 AS OF 02/12/97                      
*PHASE SPEXTOM,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE MSUNPKS                                                                
*INCLUDE MSPACKS                                                                
         TITLE 'DMLDEXTOM - FIND RECS FOR OMCO'                                 
* MEDIA R, CLT AP, PROD TG 1935 WKTU-F EST 100                                  
* MEDIA R, CLT AP, PROD TG 1935 WYNY-F EST 100                                  
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
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         CLC   0(10,R3),OMCOKEY1    SPOT BUY KEY                                
         BNE   DMXREC10                                                         
         AP    RECCT,=P'1'                                                      
         LA    R4,=CL20'OMCO AP/TG/100/WKTUF'                                   
         SR    R5,R5                                                            
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         SPACE                                                                  
         B     DMXKEEP                                                          
         SPACE                                                                  
DMXREC10 CLC   0(10,R3),OMCOKEY2    SPOT BUY KEY                                
         BNE   DMXPURGE                                                         
         AP    RECCT,=P'1'                                                      
         LA    R4,=CL20'OMCO AP/TG/100/WYNYF'                                   
         SR    R5,R5                                                            
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         SPACE                                                                  
         B     DMXKEEP                                                          
*                                                                               
DMXEOF   DS    0H                                                               
         EDIT  RECCT,(10,P+10),0,COMMAS=YES,MINUS=YES                           
         MVC   P+20(10),=C'RECS FOUND'                                          
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
OMCOKEY1 DS    0X                                                               
         DC     X'1281FFFF078FC7C12164'                                         
OMCOKEY2 DS    0X                                                               
         DC     X'1281FFFF078FCC79E164'                                         
RECCT    DC    PL3'0'                                                           
WORK     DS    CL64                                                             
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT OFF                                                              
SPGENBUYD DSECT                                                                 
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SPEXTOM   09/16/97'                                      
         END                                                                    
