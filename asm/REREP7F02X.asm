*          DATA SET REREP7F02X AT LEVEL 140 AS OF 05/01/02                      
*PHASE RE7F02A,*                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE DAYPAK                                                                 
*INCLUDE TIMVAL                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE REGENBUC                                                               
*INCLUDE RECUP                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PERVERT                                                                
*INCLUDE QSORT                                                                  
         TITLE 'REREP7F02  (RE7F02A) --- SUPER SWITCHER LISTING'                
*                                                                               
********************************************************************            
*                                                                  *            
*        REREP7F02  -- KATZ SUPER SWITCHER PREP LISTING            *            
*                                                                  *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* DEC09/95 (BU ) --- ORIGINAL ENTRY, BASED ON CONVERT  (REREPK802) *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  RUN-TIME SWITCHES AND INPUT VALUES:                             *            
*      QUESTOR    =   Y   DISPLAY SORT OUTPUT                      *            
*      QUESTOR+1  =   Y   DISPLAY SORT RETURN                      *            
*      QUESTOR+2  =   Y   DISPLAY CONTRACT OUTPUT RECORDS          *            
*      QUESTOR+3  =   Y   DISPLAY KEYS AS DELETED                  *            
*      QUESTOR+4  =   Y   DISPLAY CONTRACT SWITCH                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*      QRECORD+20  =  REPS TO SCAN FOR SWITCH:  MAX OF 8           *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
RE7F02   CSECT                                                                  
         NMOD1 0,**RE7F**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
*                                                                               
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BNE   MAIN0900                                                         
*                                                                               
MAIN0020 DS    0H                                                               
*                                                                               
         BAS   RE,INITIAL          ESTABLISH WORK AREAS                         
         BAS   RE,TABLINIT         SET UP AGENCY SWITCH TABLE                   
MAIN0900 DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*   INITIALIZATIONS ....                                                        
INITIAL  NTR1                                                                   
         SPACE 1                                                                
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',1000000,1000000                                 
*                                  GET 1MEG STORAGE SPACE                       
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVC   ARECAREA,P2         TAPE RECORD DELIVERY AREA                    
         L     RF,ARECAREA                                                      
         A     RF,=F'100'          TAPE BUFFER AREA:                            
         ST    RF,AAGYAREA         SET A(AGENCY AREA)                           
         ST    RF,ANXTAGY          SET A(NEXT AGENCY)                           
***>>>   MVC   AGYREP,=C'K3'       NO  - DEFAULT TO KATZ                        
         MVC   AGYREP,RCREPFL      SET REP CODE FOR JOB.                        
INIT0040 EQU   *                                                                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*   TABLINIT:                                                                   
*                                                                               
TABLINIT NTR1                                                                   
*                                                                               
*   AGYTABLE:  READ ALL AGENCY RECORDS.  SKIP ALL NUMERIC ENTRIES.              
*        FOR EACH CORPORATE ENTRY, SEE IF THERE IS AN EQUIVALENCY               
*        CODE.  IF THERE IS, RETRIEVE MAIN AGENCY RECORD TO GET THE             
*        AGENCY NAME.  RELEASE THE RECORD TO SORT.                              
*             BYTES 0  -  3  =  EQUIVALENCY CODE                                
*             BYTES 4  -  5  =  OFFICE CODE (SPACE IF CORPORATE)                
*             BYTES 6  -  9  =  REPLACEMENT CODE                                
*             BYTES 10 - 29  =  AGENCY NAME                                     
*                                                                               
AGYTABLE EQU   *                                                                
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         XC    SORTREC,SORTREC                                                  
         LA    RF,RECORD3          SET A(IO AREA FOR PROCEDURE)                 
         ST    RF,AIOAREA                                                       
AGYT0020 EQU   *                                                                
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'1A'           SET RECORD TYPE                              
         GOTO1 HIGH                READ FIRST RECORD                            
         B     AGYT0060                                                         
AGYT0040 EQU   *                                                                
         GOTO1 SEQ                 READ NEXT RECORD                             
AGYT0060 EQU   *                                                                
*        MVC   P+1(10),=C'KEY INPUT='                                           
*        MVC   P+13(27),KEY                                                     
*        GOTO1 REPORT                                                           
         CLI   KEY,X'1A'           AGENCY RECORD?                               
         BNE   AGYT0200            NO  - FINISHED                               
         CLC   KEY+25(2),AGYREP    SELECTED REP AGENCY RECORD?                  
         BNE   AGYT0040            NO  - SKIP THE RECORD                        
         LA    RF,KEY+19           CHECK KEY FOR ALL NUMERIC                    
         LA    RE,4                SET LOOP CONTROL                             
AGYT0080 EQU   *                                                                
         CLI   0(RF),C'0'          ZERO COMPARE                                 
         BL    AGYT0120            LESS THAN ZERO: USE IT                       
         CLI   0(RF),C'9'          9 COMPARE                                    
         BH    AGYT0120            MORE THAN 9: USE IT                          
         BCT   RE,AGYT0080         DO EACH POSITION                             
         B     AGYT0040            ALL NUMERIC: DON'T USE IT                    
AGYT0120 EQU   *                                                                
*        MVC   P+1(10),=C'SAVED KEY='                                           
*        MVC   P+13(10),SORTSAVE                                                
*        GOTO1 REPORT                                                           
         CLC   KEY+23(2),SPACES    ANY AGENCY OFFICE CODE?                      
         BNE   AGYT0040            YES- GO BACK FOR NEXT                        
         GOTO1 GREC                NO  - READ THE RECORD                        
         OC    RAGY2EQU,RAGY2EQU   ANY EQUIVALENCY CODE?                        
         BZ    AGYT0040            NO  - GO BACK FOR NEXT                       
         MVC   SORTNUM,RAGY2EQU    INSERT EQUIV CODE                            
         MVC   SORTALFA,RAGK2AGY   INSERT NEW AGENCY CODE                       
         MVC   KEYSAV2,KEY         SAVE KEY FOR RESTART                         
         MVI   KEY,X'0A'           INSERT AGENCY1 RECORD TYPE                   
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   AGYT0130            NO  - SKIP THIS RECORD                       
         GOTO1 GREC                RETRIEVE THE RECORD                          
         MVC   SORTNAME,RAGYNAM1   INSERT NAME INTO SORTREC                     
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
AGYT0130 EQU   *                                                                
         MVC   KEY,KEYSAV2         RELOAD ORIGINAL 1A KEY                       
         GOTO1 HIGH                RETRIEVE THE RECORD                          
         CLI   QUESTOR,C'Y'        DISPLAY SORT OUTPUT?                         
         BNE   AGYT0140                                                         
         MVC   P+1(09),=C'CORP OUT:'                                            
         MVC   P+12(10),SORTREC                                                 
         GOTO1 REPORT                                                           
AGYT0140 EQU   *                                                                
         B     AGYT0040            GO BACK FOR NEXT                             
AGYT0200 EQU   *                                                                
         MVI   FORCEHED,C'Y'       FORCE PAGE CHANGE                            
AGYT0240 EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4           END OF FILE?                                 
         LTR   R6,R6                                                            
         BZ    AGYT0400            YES                                          
         MVC   SORTREC,0(R6)       RETRIEVE SORT RECORD                         
         MVC   P+1(4),SORTNUM                                                   
         MVC   P+10(4),SORTALFA                                                 
         MVC   P+20(20),SORTNAME                                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     AGYT0240            GO BACK FOR NEXT                             
AGYT0400 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
ARECAREA DS    A                   TAPE RECORD DELIVERY AREA                    
LBLDAREA DS    F                                                                
AAGYAREA DS    A                   AGENCY CONVERSION AREA                       
ANXTAGY  DS    A                   NEXT AGENCY AREA                             
AAGYEND  DS    A                   A(LAST ENTRY IN TABLE)                       
ANEWAGY  DS    A                   A(NEW AGENCY FOR CONTRACT)                   
AIOAREA  DS    F                                                                
KEYSAV2  DS    CL27                SECOND KEY SAVE AREA                         
COMMAND  DS    CL8                                                              
AGYREP   DS    CL2                                                              
*                                                                               
FOXZEROS DC    C'0000000'                                                       
ELTAREA  DS    CL128                                                            
         DS    0F                                                               
SORTREC  DS    CL30                                                             
         ORG   SORTREC                                                          
SORTNUM  DS    CL4                 ORIGINAL NUMERIC AGENCY CODE                 
SORTOFF  DS    CL2                 OFFICE NUMBER                                
SORTALFA DS    CL4                 NEW ALPHANUMERIC AGENCY CODE                 
SORTNAME DS    CL20                AGENCY NAME                                  
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,6,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=30'                                    
*                                                                               
*                                                                               
*                                                                               
ELCODE   DS    CL1                                                              
SHOWIT   DS    CL1                 DISPLAY FLAG                                 
         DS    0F                                                               
         SPACE 3                                                                
*                                                                               
         SPACE 3                                                                
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL2008              AREA FOR RECORD                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
*  INCLUDE REGENBUY                BUY RECORD                                   
*  INCLUDE REGENCON                CONTRACT RECORD                              
*  INCLUDE REGENSAL                SALESPERSON RECORD                           
*  INCLUDE REGENPTP                POINT PERSON RECORD                          
*  INCLUDE REGENSTA                STATION     RECORD                           
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
FILED    DSECT                                                                  
RECORD   DS    CL2048                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENCON          CONTRACT RECORD                              
         EJECT                                                                  
RECORD3  DS    CL1024                                                           
         ORG   RECORD3                                                          
       ++INCLUDE REGENAGY          AGENCY      RECORD                           
         EJECT                                                                  
         ORG   RECORD3                                                          
       ++INCLUDE REGENAGY2         AGENCY2     RECORD                           
         EJECT                                                                  
         ORG                                                                    
RECORD4  DS    CL1024                                                           
RECORD5  DS    CL1024                                                           
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE DDCOMFACSD                                                     
         EJECT                                                                  
*********************************************************************           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'140REREP7F02X05/01/02'                                      
         END                                                                    
