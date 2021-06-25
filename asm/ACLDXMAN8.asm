*          DATA SET ACLDXMAN8  AT LEVEL 027 AS OF 10/15/18                      
*PHASE ACXMA8A                                                                  
*INCLUDE ACRECTYP                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
***********************************************************************         
* THIS EXTERNAL ADDS SERIAL NUMBER TO EACH TRANSACTION FOR EITHER ONE *         
* AGENCY OR A WHOLE FILE    DO NOT DELETE                             *         
***********************************************************************         
DMLDEXT  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         B     DMXCTL                                                           
         DC    C'*VERIFY*'                                                      
         DC    X'FB'                                                            
         DC    C'80101'                                                         
         DC    X'FB'                                                            
         DC    C'81231'                                                         
         DC    C'*',X'FF'          FILE=ALL                                     
*        DC    X'I3'               FILE=ACCI3                                   
*                                                                               
DMXRTST  DS    0H                                                               
*        B     DMXPURGE            WHEN UNDER TEST                              
         B     DMXKEEP                                                          
         EJECT                                                                  
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST(PLISTL),0(R1)                                              
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
                                                                                
DMXINIT  DS    0H                                                               
DMXEOF   DS    0H                                                               
DMXRET   DS    0H                                                               
DMXIT    XIT1  ,                                                                
         EJECT                                                                  
DMXREC   L     R2,VREC             READ RECORD FOR COMPANY AND TYPE             
         USING CPYRECD,R2                                                       
         GOTO1 VRECTYP,DMCB,(C'D',CPYRECD)                                      
         MVC   COMPNY,1(R1)        SAVE COMPANY BYTE                            
*        CLI   COMPNY,X'78'        COMPANY-CODE = X'78'                         
*        BNE   DMXRTST             NO:GO AWAY                                   
         CLI   COMPNY,X'41'        COMPANY-CODE < X'41'                         
         BL    DMXRTST             YES:GO AWAY                                  
         CLI   COMPNY,X'FE'        COMPANY-CODE > X'FE'                         
         BH    DMXRTST             YES:GO AWAY                                  
*                                                                               
DMXREC00 MVC   RECTYPE,0(R1)       SAVE RECORD EQUATED VALUE                    
         CLI   RECTYPE,ACRTCPY     IS IT COMPANY RECORD                         
         BNE   DMXREC10                                                         
         SPACE                                                                  
DMXREC02 LA    R3,CPYRFST                                                       
         XR    R0,R0                                                            
         USING CPYELD,R3                                                        
DMXREC04 CLI   CPYEL,0             DID WE FIND COMPANY ELEMENT                  
         BNE   *+6                                                              
         DC    H'0'                NO THEN DIE AS WE SHOULD                     
         CLI   CPYEL,CPYELQ                                                     
         BE    DMXREC06                                                         
         IC    R0,CPYLN                                                         
         AR    R3,R0                                                            
         B     DMXREC04                                                         
                                                                                
DMXREC06 CLI   CPYLN,CPYLN3Q       DO WE CURRENTLY HAVE STATUS 9                
         BL    DMXREC08                                                         
         OI    CPYSTAT9,CPYSSRNM   YES THEN SWITCH ON SERIAL NUMBER             
         B     DMXKEEP                                                          
*                                                                               
DMXREC08 XC    ELEMENT,ELEMENT     NO THEN REBUILD ELEMENT                      
NEW      USING CPYELD,R4                                                        
         LA    R4,ELEMENT                                                       
         IC    RF,CPYLN            COPY EXISTING ELEMENT OVER                   
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   NEW.CPYELD(0),CPYELD                                             
         MVI   NEW.CPYLN,CPYLN3Q   INCREASE LENGTH FOR STATUS 9                 
         OI    NEW.CPYSTAT9,CPYSSRNM    TURN ON SERIAL NUMBER REQUIRED          
         MVI   CPYEL,X'FF'         MARK EXISTING ELEMENT FOR DELETION           
         B     DMXREC30            GO TO HELLO ROUTINES                         
         DROP  NEW                                                              
*                                                                               
         USING TRNRECD,R2                                                       
DMXREC10 CLI   RECTYPE,ACRTTRN     IS IT TRANSACTION RECORD                     
         BE    DMXREC12            NO - GO AWAY                                 
         CLI   RECTYPE,ACRTTRNA    IS IT ARCHIVED TRANSACTION RECORD            
         BNE   DMXRTST             NO - GO AWAY                                 
*                                                                               
DMXREC12 CLC   SAVECPY,1(R1)       IS IT THE SAME COMPANY AS BEFORE             
         BE    DMXREC14            YES                                          
         MVC   SAVECPY,1(R1)                                                    
         XC    SERIAL,SERIAL       CLEAR SERIAL NUMBER TO ZERO                  
                                                                                
         USING SERELD,R4                                                        
DMXREC14 LA    R4,TRNRFST          INCREASE SERIAL NUMBER BY 1                  
         SR    R0,R0                                                            
DMXREC16 CLI   SEREL,0                                                          
         BE    DMXREC20                                                         
         CLI   SEREL,SERELQ                                                     
         BNE   DMXREC18                                                         
         MVI   SEREL,X'FF'                                                      
DMXREC18 IC    R0,SERLN                                                         
         AR    R4,R0                                                            
         B     DMXREC16                                                         
                                                                                
DMXREC20 XR    R3,R3               INCREASE SERIAL NUMBER BY 1                  
         ICM   R3,15,SERIAL        IF COMPANY THE SAME                          
         AHI   R3,1                                                             
         ST    R3,SERIAL                                                        
         XC    ELEMENT,ELEMENT     CLEAR ELEMENT                                
NEW      USING SERELD,R4                                                        
         LA    R4,ELEMENT          BUILD ELEMENT                                
         MVI   NEW.SEREL,SERELQ    TYPE OF ELEMENT                              
         MVI   NEW.SERLN,6         LENGTH OF ELEMENT                            
         MVC   NEW.SERNM,SERIAL    SERIAL NUMBER FOR THIS ELEMENT               
         DROP  NEW                                                              
*                                                                               
DMXREC30 GOTO1 VHELLO,DMCB,(C'D',ACCMST),(X'FF',TRNRECD),0,0                    
         CLI   12(R1),0           DID WE DELETE BAD ELELENTS OK                 
         BE    *+6                                                              
         DC    H'0'               NO                                            
DMXREC40 GOTO1 VHELLO,DMCB,(C'P',ACCMST),TRNRECD,ELEMENT,0                      
         CLI   12(R1),0           DID ADD NEW ELEMENT OK                        
         BE    *+8                                                              
         BNE   DMXKEEP            NO                                            
         SR    RE,RE              ADD RECORD LENGTH TO HEADER FIELD             
         ICM   RE,3,TRNRLEN       RECORD LENGTH                                 
         LA    RE,4(RE)                                                         
         LA    RF,TRNRECD                                                       
         SH    RF,=H'4'           FIND HEADER FIELD                             
         SLL   RE,16                                                            
         STCM  RE,15,0(RF)        MOVE IN LENGTH                                
         B     DMXKEEP                                                          
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
VRECTYP  DC    V(ACRECTYP)                                                      
VHELLO   DC    V(HELLO)                                                         
ACCMST   DC    C'ACCMST  '                                                      
SAVECPY  DC    X'00'                                                            
         DS    0F                                                               
SERIAL   DC    XL4'00'                                                          
                                                                                
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
WORK     DS    XL64                                                             
*                                                                               
PLIST    DS    0X                                                               
VREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
VCARDS   DS    V                                                                
VPEELDT  DS    A                                                                
VISREC   DS    A                                                                
PLISTL   EQU   *-PLIST                                                          
*                                                                               
RECTYPE  DS    X                                                                
COMPNY   DS    X                                                                
ELEMENT  DS    XL255                                                            
         DS    0D                                                               
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027ACLDXMAN8 10/15/18'                                      
         END                                                                    
