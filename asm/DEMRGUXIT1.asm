*          DATA SET DEMRGUXIT1 AT LEVEL 006 AS OF 10/20/15                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEMRGU1A                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
         TITLE 'MERGE DFSORT EXIT: READ DEMO FILE'                              
***********************************************************************         
*                                                                     *         
* THIS IS A DFSORT USER EXIT PROGRAM. IT IS INVOKED VIA MEMBER        *         
* DEMRGICEG, WHICH CONTAINS CONTROL CARDS TO ICETOOL/DFSORT.          *         
* SEE IBM'S DFSORT: APPLICATION PROGRAMMING GUIDE FOR DETAILS.        *         
*                                                                     *         
* NOTE: CALLS TO EXTERNAL SUBROUTINES *MUST* BE MADE VIA              *         
*       CALL_DDS_SUBRTN, BECAUSE THIS EXIT'S USE OF REGISTERS IS      *         
*       NON-DDS-CONFORMING.                                           *         
*                                                                     *         
* THIS MODULE IS A COMPONENT OF THE GENERALIZED DEMO MERGE UTILITY.   *         
* IT READS A DATASET WHICH HAS BEEN PRODUCED BY A CONVERSION PROGRAM, *         
* BUT WHICH HAS HAD ALL DUPLICATE MAJOR KEYS REMOVED. THIS PROGRAM    *         
* THEN READS THE DEMO FILE FOR ALL EXISTING RECORDS FOR EACH MAJOR    *         
* KEY IN THE INPUT FILE. THOSE DEMO RECORDS ARE RETURNED TO DFSORT.   *         
*                                                                     *         
***********************************************************************         
DEMRGU1  CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ENTRY E35                 MUST BE "E35" (FOR DFSORT)                   
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         REQUS                                                                  
*                                                                               
         USING E35,RC              RC = PROGRAM BASE REGISTER                   
E35      STM   RE,RC,12(RD)        SAVE ALL REGS EXCEPT RD                      
         LA    RC,0(RF)            SET PROGRAM BASE REGISTER                    
         STMH  GR0,GRF,DFSORT_HIGH_HALVES                                       
         ST    RD,SAVE15+4         SAVE BACKWARD POINTER                        
         LA    RE,SAVE15           SET FORWARD POINTER...                       
         ST    RE,8(RD)            ...IN SAVE AREA                              
         LR    RD,RE               SET OUR SAVE AREA                            
         LR    R2,R1               SAVE PARAMETER LIST POINTER                  
*                                                                               
         BC    0,MAIN10            *** SELF-MODIFYING CODE ***                  
         MVI   *-3,X'F0'           *** ONLY DO THIS ONCE   ***                  
*                                                                               
GETCARD  DS    0H                                                               
         GOTOR ,DMCB,CARD,=C'RE00'                                              
         L     RF,=V(CARDS)                                                     
         BRAS  RE,CALL_DDS_SUBRTN                                               
         CLC   =C'/*',CARD         ANY MORE PARAMETER CARDS?                    
         BE    EOFSYSIN            NO                                           
*                                                                               
         CLC   =C'DDSIO=',CARD     CHECK FOR DDSIO= CARD                        
         BNE   *+18                                                             
         ICM   RF,15,=V(DDSIO)     USE ALTERNATE DDSIO                          
         MVC   0(8,RF),CARD+6                                                   
         B     GETCARD             GET NEXT PARAMETER CARD                      
*                                                                               
         CLC   =C'DSPACE=',CARD    CHECK FOR DSPACE= CARD                       
         BNE   *+18                                                             
         LA    RF,SSB                                                           
         MVC   SSODSPAC-SSOOFF(,RF),CARD+7   DSPACE=X                           
         B     GETCARD             GET NEXT PARAMETER CARD                      
*                                                                               
         CLC   =C'READFILE=DEMFILE',CARD                                        
         BE    GETCARD             IT'S THE DEFAULT: GET NEXT CARD              
         CLC   =C'READFILE=PAVFILE',CARD                                        
         BE    *+6                                                              
         DC    H'0'                INVALID PARAMETER CARD                       
         MVC   ISFILE,=CL8'PAVDIR'                                              
         MVC   DAFILE,=CL8'PAVFIL'                                              
         B     GETCARD             GET NEXT PARAMETER CARD                      
*                                                                               
EOFSYSIN DS    0H                                                               
         GOTOR ,DMCB,(0,=C'DMOPEN'),DMSYS,DMFLIST                               
         L     RF,=V(DATAMGR)                                                   
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
MAIN10   DS    0H                                                               
         L     R3,0(R2)            LOAD A(RECORD)                               
         LTR   R3,R3               EOF?                                         
         BZ    EOF                 YES: DO NOT RETURN                           
*                                                                               
         CLI   MOREFLAG,C'Y'       ARE WE READING MINOR KEYS NOW?               
         BE    SEQ                 YES: READ THE NEXT ONE                       
*                                                                               
         LA    R3,4(R3)            POINT PAST RDW                               
K        USING DRECORD,KEY                                                      
         MVC   K.DMAJKEY,0(R3)     MAJOR KEY                                    
         GOTOR ,DMCB,(0,=C'DMREAD'),ISFILE,KEY,KEY                              
         L     RF,=V(DATAMGR)                                                   
         BRAS  RE,CALL_DDS_SUBRTN                                               
         CLI   8(R1),X'10'         RECORD NOT FOUND?                            
         BE    DELREC              CORRECT: NO MINOR KEYS                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                FATAL DATAMGR ERROR                          
*                                                                               
R        USING DRECORD,OUT_REC                                                  
         MVC   R.DMAJKEY,K.DMAJKEY      SET MAJOR KEY                           
         XC    R.DRMINKEY,R.DRMINKEY    CLEAR MINOR KEY                         
         MVC   R.DRSTATUS,K.DKSTATUS    SET STATUS BYTE                         
         GOTOR ,DMCB,(0,=C'DMRDHI'),DAFILE,K.DKDXDA,OUT_REC                     
         L     RF,=V(DATAMGR)                                                   
         BRAS  RE,CALL_DDS_SUBRTN                                               
         B     CHKEOFMN                                                         
*                                                                               
SEQ      DS    0H                                                               
         GOTOR ,DMCB,(0,=C'DMRSEQ'),DAFILE,K.DKDXDA,OUT_REC                     
         L     RF,=V(DATAMGR)                                                   
         BRAS  RE,CALL_DDS_SUBRTN                                               
         DROP  K                                                                
*                                                                               
CHKEOFMN DS    0H                                                               
         CLI   8(R1),X'80'         EOF?                                         
         BE    DELREC              YES: ALL MINOR KEYS HAVE BEEN READ           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                  PUT THE RECORD TO THE OUTPUT FILE            
         SR    R0,R0                                                            
         ICM   R0,3,R.DRRECLEN                                                  
         AHI   R0,4                FOR RDW                                      
         STCM  R0,3,OUT_RDW                                                     
         MVI   MOREFLAG,C'Y'       DO A READ SEQUENTIAL UPON RETURN             
         B     ADDREC                                                           
         DROP  R                                                                
*                                                                               
*                                                                               
ADDREC   DS    0H                                                               
         LGHI  GRF,12              SET RC=12: INSERT RECORD                     
         SGR   GR1,GR1                                                          
         LA    R1,OUT_RDW          SET RECORD POINTER                           
         B     GOBACK                                                           
*                                                                               
DELREC   DS    0H                                                               
         MVI   MOREFLAG,C'N'       THIS MAJOR KEY IS DONE                       
         LGHI  GRF,4               SET RC=4: DELETE RECORD                      
         B     GOBACK                                                           
*                                                                               
EOF      DS    0H                                                               
         GOTOR ,DMCB,(0,=C'DMCLSE'),DMSYS,DMFLIST                               
         L     RF,=V(DATAMGR)                                                   
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
         LGHI  GRF,8               SET RC=8:EOF                                 
*                                                                               
GOBACK   DS    0H                                                               
         LMH   GR0,GR0,DFSORT_HIGH_HALVES                                       
         LMH   GR2,GRE,DFSORT_HIGH_HALVES+8                                     
         L     RD,4(,RD)                                                        
         L     RE,12(,RD)                                                       
         LM    R2,RC,28(RD)        RESTORE REGS                                 
         BSM   0,RE                RETURN                                       
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
* CALL A DDS SUBROUTINE. THIS REQUIRES ESTABLISHING A NEW RD CHAIN,             
* BECAUSE THIS PROGRAM IS A DFSORT EXIT, AND DOES NOT CONFORM TO DDS            
* STANDARD REGISTER USAGE.                                                      
*                                                                               
* INPUT REGISTERS ARE STANDARD:                                                 
*   R1 = A(PARAMETER LIST)                                                      
*   RE = RETURN ADDRESS                                                         
*   RF = A(ROUTINE TO CALL)                                                     
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
CALL_DDS_SUBRTN DS 0H                                                           
         STM   RE,RC,GPRSAVE       SAVE CALLER'S REGISTERS                      
         LR    R0,RD               SAVE CALLER'S RD LOCALLY                     
         DROP  RC                                                               
*                                                                               
         BASR  RB,0                                                             
         AHI   RB,-2                                                            
         USING *-6,RB              MAKE THIS ROUTINE ADDRESSABLE                
         L     RD,=V(REGSAVE)      DDS WORKING STORAGE                          
*                                                                               
         BASR  RE,RF               CALL EXTERNAL SUBROUTINE                     
*                                                                               
         LR    RD,R0               RESTORE CALLER'S RD                          
         LM    RE,RC,GPRSAVE       RESTORE CALLER'S REGISTERS                   
         BSM   0,RE                EXIT                                         
         DROP  RB                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
DMSYS    DC    CL8'DEMO'                                                        
*                                                                               
DMFLIST  DC    0D                  OPEN ALL DEMO FILES AND DIRECTORIES          
         DC    C'NDEMDIRN'                                                      
         DC    C'NL=DEMFN'                                                      
         DC    C'NDEMDIRA'                                                      
         DC    C'NL=DEMFA'                                                      
         DC    C'NDEMDIRR'                                                      
         DC    C'NL=DEMFR'                                                      
         DC    C'NNTIDIR '                                                      
         DC    C'NL=NTIFL'                                                      
         DC    C'NPAVDIR '                                                      
         DC    C'NL=PAVFL'                                                      
         DC    C'X'                                                             
         SPACE 2                                                                
         ORG   DEMRGU1+(((*-DEMRGU1)/256)+1)*256  FOR I-CACHE PIPELINE          
         SPACE 2                                                                
         DS    0L                                                               
         DC    CL16'UTL*UTL*UTL*UTL*'                                           
UTL      DC    F'0'                                                             
         DC    X'0C'               UTL FOR DEMOS                                
         DC    XL251'00'                                                        
*                                                                               
         DS    0L                                                               
         DC    CL16'SSB*SSB*SSB*SSB*'                                           
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSOMTIND            SYSTEM DATAMGR FLAGS                         
         DC    AL1(SSOWRTN)        WRITE=NO (DON'T OPEN FOR UPDATE)             
         ORG                                                                    
         EJECT                                                                  
SAVE15   DS    18F                 SAVE DFSORT'S REGISTERS                      
GPRSAVE  DS    15F                 INTERNAL CALLER'S SAVED RE-RC                
DFSORT_HIGH_HALVES DS 16F                                                       
DMCB     DS    6F                  PARAMETERS TO CALL_DDS_SUBRTN                
CARD     DS    CL80                                                             
KEY      DS    CL(DKEYLENQ)                                                     
*                                                                               
MOREFLAG DC    C'N'                'Y' = WE'RE READING MINOR KEYS NOW           
*                                                                               
ISFILE   DC    CL8'DEMDIR'         READ DEMDIR/DEMFIL BY DEFAULT                
DAFILE   DC    CL8'DEMFIL'                                                      
*                                                                               
         DS    0L                                                               
         DC    C'*OUTREC*'                                                      
OUT_RDW  DC    F'0'                                                             
OUT_REC  DS    2000X                                                            
*                                                                               
         EJECT                                                                  
* GENERIC DEMO KEY/RECORD LAYOUT                                                
*                                                                               
DRECORD  DSECT                                                                  
DMAJKEY  DS    XL18                MAJOR KEY                                    
DKSTATUS DS    X                   KEY: STATUS BYTE                             
DKDXDA   DS    XL4                 KEY: DISK ADDRESS                            
DKEYLENQ EQU   *-DMAJKEY                                                        
         ORG   DKSTATUS                                                         
DRMINKEY DS    XL2                 RECORD: MINOR KEY                            
DRRECLEN DS    XL2                 RECORD: RECORD LENGTH                        
DRSTATUS DS    X                   RECORD: STATUS BYTE                          
DRELEMS  DS    0X                  RECORD: FIRST DEMO ELEMENT                   
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006DEMRGUXIT110/20/15'                                      
         END                                                                    
