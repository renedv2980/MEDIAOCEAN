*          DATA SET SPLDEXTWHO AT LEVEL 051 AS OF 05/04/99                      
*PHASE SPEXTSPF                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'SPLDEXTWHO - DIG UP REVISION RECORDS AND 1ST ELEM'              
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
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R6,AREC                                                          
         CLC   =X'0D6B',0(R6)                                                   
         BNE   DMXKEEP                                                          
         USING NBRKEY,R6                                                        
         OC    NBRKKBUY,NBRKKBUY   OFF A BUYLINE?                               
         BZ    DMXKEEP                                                          
         LA    R6,NBRFSTEL         ADDRESS OF FIRST ELEMENT                     
         DROP  R6                                                               
         USING NBRSELD,R6                                                       
         CLI   0(R6),NBRSELQ                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBRSBYLN,0          IS THERE A BUYLINE NUMBER HERE?              
         BNE   DMXKEEP                                                          
*                                                                               
         L     R1,COUNT            NUMBER OF BUY RECORDS READ                   
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
*                                                                               
         MVI   P,C'='                                                           
         MVC   P+1(39),P                                                        
         GOTO1 VPRINTER                                                         
         GOTO1 VHEXOUT,DMCB,AREC,P,13,=C'TOG'                                   
         GOTO1 VPRINTER                                                         
         GOTO1 VHEXOUT,DMCB,(R6),P,16,=C'TOG'                                   
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
                                                                                
DMXEOF   DS    0H                                                               
         MVI   P,C'*'                                                           
         MVC   P+1(39),P                                                        
         GOTO1 VPRINTER                                                         
         MVC   P(13),=C'RECORDS MATCH'                                          
         EDIT  (4,COUNT),(8,P+25),ZERO=NOBLANK                                  
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
                                                                                
EXIT     XIT1                                                                   
***********************************************************************         
* UPDTAM: UPDATE THE A/M CODES IN TABLE                               *         
***********************************************************************         
         SPACE                                                                  
UPDTAM   NTR1                                                                   
         LA    R2,BYRTABLE                                                      
         USING BYRD,R2                                                          
                                                                                
UPDTAM10 CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    UPDTAMX                                                          
                                                                                
         CLC   0(1,R2),EBCMD       SAME MEDIA?                                  
         BNE   *+10                                                             
         MVC   BYRAGYMD,BAGYMD     YES, PUT IN TABLE                            
                                                                                
         LA    R2,BYRLNQ(R2)       NEXT ENTRY                                   
         B     UPDTAM10                                                         
                                                                                
         DROP  R2                                                               
UPDTAMX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* UPDTBY: UPDATE THE BINARY BUYER CODES IN TABLE                      *         
***********************************************************************         
         SPACE                                                                  
UPDTBY   NTR1                                                                   
         MVI   DELETE,C'N'                                                      
         LA    R2,BYRTABLE                                                      
         USING BYRD,R2                                                          
                                                                                
UPDTBY10 CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    UPDTBYX                                                          
                                                                                
         CLC   EBCBYR,BYREB        SAME EBCDIC BUYER CODE                       
         BNE   UPDTBY20                                                         
         CLC   BAGYMD,BYRAGYMD     SAME BINARY A/M                              
         BE    UPDTBY30                                                         
                                                                                
UPDTBY20 LA    R2,BYRLNQ(R2)       NEXT ENTRY                                   
         B     UPDTBY10                                                         
                                                                                
UPDTBY30 MVC   BYRBIN,BINBYR       YES, SAVE BINARY BUYER CODE                  
         MVI   DELETE,C'Y'                                                      
                                                                                
         DROP  R2                                                               
UPDTBYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CHKTAB: CHECK THAT TABLE IS COMPLETE                                *         
***********************************************************************         
         SPACE                                                                  
CHKTAB   NTR1                                                                   
         MVI   CHKDTBLE,C'Y'       TABLE HAS BEEN CHECKED                       
         LA    R2,BYRTABLE                                                      
         USING BYRD,R2                                                          
                                                                                
CHKTAB10 CLI   0(R2),X'FF'                                                      
         BE    CHKTABX                                                          
                                                                                
         CLI   BYRAGYMD,0          A/M CODE = 0?                                
         BE    CHKTAB20                                                         
         CLI   BYRBIN,0            BINARY BUYER CODE = 0?                       
         BNE   CHKTAB30                                                         
                                                                                
CHKTAB20 MVC   P(11),=C'TABLE ERROR'                                            
         MVC   P+15(1),BYRMD                                                    
         MVC   P+17(3),BYREB                                                    
         GOTO1 VPRINTER                                                         
                                                                                
CHKTAB30 MVC   P(1),BYRMD          BUYER MEDIA                                  
         MVC   P+2(3),BYREB        EBCDIC BUYER CODE                            
         GOTO1 VHEXOUT,DMCB,BYRAGYMD,P+6,1,=C'TOG'                              
         GOTO1 VHEXOUT,DMCB,BYRBIN,P+8,1,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         LA    R2,BYRLNQ(R2)                                                    
         B     CHKTAB10                                                         
                                                                                
         DROP  R2                                                               
CHKTABX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LOOKUP: LOOKUP BINARY AGENCY IN TABLE                               *         
***********************************************************************         
         SPACE                                                                  
LOOKUP   NTR1                                                                   
         MVI   DELETE,C'N'                                                      
                                                                                
         LA    R2,BYRTABLE                                                      
         USING BYRD,R2                                                          
                                                                                
LOOKUP10 CLI   0(R2),X'FF'                                                      
         BE    LOOKUPX                                                          
                                                                                
         CLC   BINBYR,BYRBIN       SAME BINARY BUYER CODE?                      
         BNE   LOOKUP20                                                         
         CLC   BAGYMD,BYRAGYMD     SAME BINARY A/M?                             
         BE    LOOKUP30                                                         
                                                                                
LOOKUP20 LA    R2,BYRLNQ(R2)                                                    
         B     LOOKUP10                                                         
                                                                                
LOOKUP30 MVI   DELETE,C'Y'                                                      
                                                                                
         DROP  R2                                                               
LOOKUPX  B     EXIT                                                             
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
*                                                                               
BYTE     DS    X                                                                
COUNT    DS    F                                                                
EBCMD    DS    C                   EBCDIC MEDIA                                 
EBCBYR   DS    CL3                 EBCDIC BUYER CODE                            
BINBYR   DS    X                   BINARY BUYER CODE                            
BAGYMD   DS    X                   BINARY AGENCY MEDIA                          
DELETE   DS    C                   DELETE THE RECORD FLAG                       
CHKDTBLE DS    C                   ALREADY CHECKED THE TABLE FLAG               
ELCODE   DS    X                                                                
         LTORG                                                                  
* MEDIA(1), EBCDIC BUYER CODE(3), BINARY A/M(1), BINARY BUYER CODE(1)           
BYRTABLE DS    0H                                                               
         DC    C'T',C'KKW',XL2'0000'                                            
         DC    C'T',C'LAH',XL2'0000'                                            
         DC    C'T',C'LMB',XL2'0000'                                            
         DC    C'T',C'NLD',XL2'0000'                                            
         DC    C'T',C'TW ',XL2'0000'                                            
         DC    X'FF'                                                            
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
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT OFF                                                              
AGYHDRD   DSECT                                                                 
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPNWSBYR                                                       
       ++INCLUDE SPNWSBRV                                                       
BYRD     DSECT                                                                  
BYRMD    DS    C                   EBCDIC MEDIA                                 
BYREB    DS    CL3                 EBCDIC BUYER CODE                            
BYRAGYMD DS    X                   BINARY AGENCY MEDIA                          
BYRBIN   DS    X                   BINARY BUYER CODE                            
BYRLNQ   EQU   *-BYRMD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051SPLDEXTWHO05/04/99'                                      
         END                                                                    
