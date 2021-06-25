*          DATA SET PPEZF99    AT LEVEL 125 AS OF 10/15/97                      
*PHASE T43099B,*                                                                
         TITLE 'PPEZF99 - CONVERT JWT CPP DATA FOR DOWNLOAD - INIT'             
***********************************************************************         
*                                                                     *         
*        NOT RELATED TO EPIC SYSTEM                                   *         
*           THIS PROGRAM IS JUST MAKING USE OF SPOOF CONTROLLER       *         
*                                                                     *         
*        ROUTINE READS DATASET OF COSTS PRODUCED BY THE JWT           *         
*         MEDIA PLANNING SYSTEM AND PUTS DATA ON THE FACPAK           *         
*         PRINT QUEUE IN DOWNLOAD FORMAT SO USERS CAN DOWNLOAD        *         
*         IT INTO A LOTUS SPREADSHEET.                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PPEZF99 CSECT                                                                   
         PRINT NOGEN                                                            
         NMOD1 0,T43099**                                                       
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         TITLE 'PPEZF99 - CONVERT JWT CPP DATA FOR DOWNLOAD - MODE'             
***********************************************************************         
*                                                                     *         
*        ANALYZE CALLING MODE                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
         CLI   MODE,VALKEY         VALIDATION                                   
         BE    VALREQ                                                           
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PRTREP                                                           
*                                                                               
XIT      XIT1  ,                                                                
*                                                                               
         TITLE 'PPEZF99 - CONVERT JWT CPP DATA FOR DOWNLOAD - VALREQ'           
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE REPORT REQUEST SCREEN                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VALREQ   DS    0H                                                               
*                                                                               
VALREQX  B     XIT                                                              
*                                                                               
         TITLE 'PPEZF99 - CONVERT JWT CPP DATA FOR DOWNLOAD - PRTREP'           
***********************************************************************         
*                                                                     *         
*        ROUTINE TO GENERATE DOWNLOAD DATA                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRTREP   DS    0H                                                               
*                                                                               
         LA    R5,P1               ESTABLISH PRINTING AREA                      
         USING REPD,R5                                                          
*                                                                               
***********************************************************************         
*                                                                     *         
*        OPEN COST PER POINT TAPE                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PROPEN   DS    0H                                                               
*                                                                               
         L     R2,=A(EXTRACT)      POINT TO TAPE DCB                            
         OPEN  ((R2),INPUT)        OPEN IT AS INPUT                             
*                                                                               
PROPENX  DS    0H                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        LOOP TO READ NEXT REPORT ON TAPE AND PROCESS                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRRPTLP  DS    0H                                                               
*                                                                               
         L     R1,=A(EXTRACT)      POINT TO TAPE DCB                            
         L     R2,=A(TPBUFFER)     POINT TO INPUT AREA                          
         XC    0(L'TPBUFFER,R2),0(R2)  CLEAR AREA                               
*                                                                               
         GET   (R1),(R2)           GET USERID RECORD                            
*                                                                               
         PACK  DUB,0(5,R2)         GET USRID FOR REPORT                         
         CVB   RF,DUB              CVB                                          
         STCM  RF,3,REPUSRID       UPDATE USERID                                
*                                                                               
         L     R1,=A(EXTRACT)      POINT TO TAPE DCB                            
         L     R2,=A(TPBUFFER)     POINT TO INPUT AREA                          
         XC    0(L'TPBUFFER,R2),0(R2)  CLEAR AREA                               
         GET   (R1),(R2)           GET FIRST CPP RECORD                         
*                                                                               
         USING CPPD,R2             ESTABLISH CPP RECORD                         
*                                                                               
         MVC   REPDESC(3),CPPRCLT  SET CLIENT                                   
         MVC   REPDESC+3(8),CPPRDEM SET DEMO                                    
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        INITIALIZE REMOTE AREA                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRREMOT  EQU   *                                                                
*                                                                               
*                                                                               
         ICM   R1,15,TWAMASTC           POINT TO MASTC                          
*                                                                               
         ICM   R1,15,MCVREMOT-MASTD(R1) ESTABLISH REMOTE AREA                   
         USING REMOTED,R1                                                       
*                                                                               
         MVC   REMOTKEY(L'REPDESC),REPDESC    SET REPORT ID                     
         MVC   REMOTDST,REPUSRID   SET DESTINATION                              
*                                                                               
         CLI   REMOTLPP,68         FORCE SOME CHANGE IN REMOTE AREA             
         BNE   *+12                 THIS SHOULD FORCE THE OPENING OF            
         MVI   REMOTLPP,66          A NEW REPORT                                
         B     *+8                                                              
         MVI   REMOTLPP,68                                                      
*                                                                               
         MVC   REMOTJID,=C'CPP'                                                 
*                                                                               
         MVI   P1,C'*'             DUMMY FIRST PAGE FOR REPORT                  
         MVC   P1+20(20),=CL20'CPP DOWNLOAD'                                    
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT IT                                     
*                                                                               
         MVI   LINE,100            FORCE NEW PAGE LATER                         
*                                                                               
         L     R2,=A(TPBUFFER)     POINT TO INPUT AREA                          
         USING CPPD,R2             ESTABLISH CPP RECORD                         
*                                                                               
         MVI   RPDLDEMS,DBLQUOTE   SET FIELD SEPARATOR                          
         MVC   RPDLDEM,SPACES      SET DEMO PLACE HOLDER                        
         MVI   RPDLDEME,DBLQUOTE   SET FIELD SEPARATOR                          
*                                                                               
         MVI   RPDLMKTS,DBLQUOTE   SET FIELD SEPARATOR                          
         MVC   RPDLMKT,SPACES      SET MARKET NUMBER PLACE HOLDER               
         MVI   RPDLMKTE,DBLQUOTE   SET FIELD SEPARATOR                          
*                                                                               
         MVI   RPDLMKNS,DBLQUOTE   SET FIELD SEPARATOR                          
         MVC   RPDLMKN(7),=C'CLIENT=' SET CLIENT ID                             
         MVC   RPDLMKN+7(3),CPPRCLT   SET CLIENT CODE                           
         CLC   CPPRPRD,SPACES      CHECK FOR PRODUCT                            
         BNH   *+16                                                             
         MVC   RPDLMKN+10(9),=C',PRODUCT='  IDENTIFY PRODUCT                    
         MVC   RPDLMKN+19(3),CPPRPRD                                            
         MVI   RPDLMKNE,DBLQUOTE   SET FIELD SEPARATOR                          
*                                                                               
         MVI   RPDLDPTS,DBLQUOTE   SET FIELD SEPARATOR                          
         MVC   RPDLDPT,SPACES      SET DAYPART SPACE HOLDER                     
         MVI   RPDLDPTE,DBLQUOTE   SET FIELD SEPARATOR                          
*                                                                               
         LA    R0,4                FOUR QUARTERS OF DATA                        
         LA    R1,RPDLCPP1                                                      
         USING RPDLCPP1,R1         ESTABLISH PRINT AREA                         
         LA    R4,CPPRYR1                                                       
         USING CPPRYR1,R4          ESTABLISH QUARTER'S DATA                     
*                                                                               
PRTPQTRL DS    0H                                                               
*                                                                               
         MVC   RPDLCPP1(6),=C'" Q  "' SET QUARTER IDENTIFIER                    
         MVC   RPDLCPP1+1(1),CPPRQTR1                                           
         MVC   RPDLCPP1+3(2),CPPRYR1+2                                          
*                                                                               
PRTPQTRC DS    0H                                                               
*                                                                               
         LA    R4,CPPRYR2-CPPRYR1(R4)  NEXT QTR'S DATA                          
         LA    R1,RPDLCPP2-RPDLCPP1(R1)  NEXT QUARTER'S DATA                    
*                                                                               
         BCT   R0,PRTPQTRL                                                      
*                                                                               
         DROP  R4,R1                                                            
*                                                                               
PRTPQTRD DS    0H                                                               
*                                                                               
         MVI   RPDLEOL,DNLDEOL     INDICATE END OF LINE                         
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT IT                                     
*                                                                               
         MVI   RPDLDEMS,DBLQUOTE   SET FIELD SEPARATOR                          
         MVC   RPDLDEM,=CL8'  DEMO  ' DEMO COLUMN HEADING                       
         MVI   RPDLDEME,DBLQUOTE   SET FIELD SEPARATOR                          
*                                                                               
         MVI   RPDLMKTS,DBLQUOTE   SET FIELD SEPARATOR                          
         MVC   RPDLMKT,=C'DMA#'    SET MARKET NUMBER COLUMN HEADING             
         MVI   RPDLMKTE,DBLQUOTE   SET FIELD SEPARATOR                          
*                                                                               
         MVI   RPDLMKNS,DBLQUOTE   SET FIELD SEPARATOR                          
         MVC   RPDLMKN(3),=C'DMA'  SET MARKET NAME COLUMN HEADING               
         MVI   RPDLMKNE,DBLQUOTE   SET FIELD SEPARATOR                          
*                                                                               
         MVI   RPDLDPTS,DBLQUOTE   SET FIELD SEPARATOR                          
         MVC   RPDLDPT,=C'DPT'     SET DAYPART COLUMN HEADING                   
         MVI   RPDLDPTE,DBLQUOTE   SET FIELD SEPARATOR                          
*                                                                               
         LA    R0,4                FOUR QUARTERS OF DATA                        
         LA    R1,RPDLCPP1                                                      
         USING RPDLCPP1,R1         ESTABLISH PRINT AREA                         
*                                                                               
         MVC   RPDLCPP1(5),=C'"CPP"'  SET CPP COLUMN HEADING                    
         LA    R1,RPDLCPP2-RPDLCPP1(R1)  NEXT QUARTER'S DATA                    
         BCT   R0,*-10                                                          
*                                                                               
         DROP  R1                                                               
*                                                                               
         MVI   RPDLEOL,DNLDEOL     INDICATE END OF LINE                         
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT IT                                     
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        LOOP TO READ NEXT RECORD ON TAPE AND PROCESS                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         B     PRTPLP10            FIRST READ ALREADY DONE                      
*                                                                               
PRTPLOOP EQU   *                                                                
*                                                                               
         L     R1,=A(EXTRACT)      POINT TO TAPE DCB                            
         L     R2,=A(TPBUFFER)     POINT TO INPUT AREA                          
         XC    0(L'TPBUFFER,R2),0(R2)  CLEAR AREA                               
*                                                                               
         GET   (R1),(R2)           GET NEXT RECORD                              
*                                                                               
PRTPLP10 EQU   *                                                                
*                                                                               
         USING CPPD,R2             ESTABLISH CPP RECORD                         
*                                                                               
         CLC   =C'END',0(R2)       END OF REPORT TEST                           
         BE    PRTPLPDN                                                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        FORMAT REPORT LINE                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RPFORM   DS    0H                                                               
*                                                                               
         MVI   RPDLDEMS,DBLQUOTE   SET FIELD SEPARATOR                          
*                                                                               
         CLC   CPPRDEM,SAVDEM      CHECK FOR CHANGE OF DEMO                     
         BE    *+16                                                             
         MVC   SAVDEM,CPPRDEM      RESET LAST DEMO USED                         
         MVC   RPDLDEM,CPPRDEM     SET DEMO                                     
*                                                                               
         MVI   RPDLDEME,DBLQUOTE   SET FIELD SEPARATOR                          
*                                                                               
         MVI   RPDLMKTS,DBLQUOTE   SET FIELD SEPARATOR                          
         MVI   RPDLMKNS,DBLQUOTE   SET FIELD SEPARATOR                          
*                                                                               
         CLC   CPPRMKT,SAVMKT      CHECK FOR CHANGE OF MARKET                   
         BE    *+10                                                             
         MVC   SAVMKT,CPPRMKT      RESET LAST MARKET USED                       
*                                                                               
         MVC   RPDLMKT,CPPRMKT     SET MARKET NUMBER                            
         MVC   RPDLMKN,CPPRMKN     SET MARKET NAME                              
*                                                                               
         MVI   RPDLMKTE,DBLQUOTE   SET FIELD SEPARATOR                          
         MVI   RPDLMKNE,DBLQUOTE   SET FIELD SEPARATOR                          
*                                                                               
*                                                                               
         MVI   RPDLDPTS,DBLQUOTE   SET FIELD SEPARATOR                          
         MVC   RPDLDPT,CPPRDPT     SET DAYPART                                  
         MVI   RPDLDPTE,DBLQUOTE   SET FIELD SEPARATOR                          
*                                                                               
         LA    R0,4                FOUR QUARTERS OF DATA                        
         LA    R1,RPDLCPP1                                                      
         USING RPDLCPP1,R1         ESTABLISH QUARTER'S DATA                     
         LA    R4,CPPRYR1                                                       
         USING CPPRYR1,R4          ESTABLISH QUARTER'S DATA                     
*                                                                               
PRTPCPPL DS    0H                                                               
*                                                                               
         MVC   RPDLCPP1,CPPRCPP1   SET CPP                                      
*                                                                               
PRTPCPPC DS    0H                                                               
*                                                                               
         LA    R4,CPPRYR2-CPPRYR1(R4)  NEXT QTR'S DATA                          
         LA    R1,RPDLCPP2-RPDLCPP1(R1)  NEXT QUARTER'S DATA                    
*                                                                               
         BCT   R0,PRTPCPPL                                                      
*                                                                               
         DROP  R4,R1                                                            
*                                                                               
PRTPCPPD DS    0H                                                               
*                                                                               
         MVI   RPDLEOL,DNLDEOL     INDICATE END OF LINE                         
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT IT                                     
*                                                                               
PRTPLPCN DS    0H                                                               
*                                                                               
         B     PRTPLOOP            READ NEXT RECORD                             
*                                                                               
PRTPLPDN DS    0H                                                               
*                                                                               
         MVC   P1,SPACES                                                        
         MVI   P1,DNLDEOF          INDICATE END OF FILE                         
*                                                                               
         MVI   LINE,1              RESET LINE COUNTER FOR DOWNLOAD              
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT IT                                     
*                                                                               
         XC    SAVFLDS,SAVFLDS     INIT SAVEAREA                                
*                                                                               
PRRPTCN  DS    0H                                                               
         B     PRRPTLP                                                          
*                                                                               
PRRPTDN  DS    0H                  END OF FILE                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        CLOSE FILES                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRCLOSE  DS    0H                                                               
*                                                                               
*        COST PER POINT TAPE                                                    
*                                                                               
         L     R2,=A(EXTRACT)      POINT TO TAPE DCB                            
         CLOSE (R2)                CLOSE IT                                     
*                                                                               
PRCLOSEX DS    0H                                                               
*                                                                               
         B     XIT                                                              
         SPACE 2                                                                
DBLQUOTE EQU   C'"'                DOUBLE QUOTE FOR LOTUS                       
DNLDEOL  EQU   X'5E'               END OF LINE = SEMI-COLON                     
DNLDEOF  EQU   C':'                END OF FILE                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
REPDESCL DC    C'DOWNLOAD'                                                      
REPDESC  DS    CL20                REPORT DESCRIPTION                           
REPUSRID DS    XL2                 REPORT USER ID                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        LOCAL WORKING STORAGE                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
MDSPOOK  DS    XL(SPOOKL)          SPOOK BUILD AREA                             
MDREQHDR DS    XL(REQEOH-REQHDRD)  REQUEST HEADER BUILD AREA                    
MDREQREC DS    XL80                80 BYTE REQUEST RECORD                       
         DS    0D                                                               
SAVFLDS  DS    0XL12               LAST USED FIELD VALUES                       
SAVDEM   DS    CL8                 LAST USED DEMO                               
SAVMKT   DS    CL4                 LAST USED MARKET CODE                        
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        DCB INFORMATION                                              *         
***********************************************************************         
         SPACE 1                                                                
EXTRACT  DCB   DDNAME=EXTRACT,DSORG=PS,EODAD=PRRPTDN,MACRF=GM,         X        
               RECFM=FB,LRECL=150,BLKSIZE=1500                                  
TPBUFFER DS    XL150               TAPE INPUT BUFFER                            
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        REPORT LINES FOR DOWNLOADING                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
REPD     DSECT                                                                  
RPDWNLN1 DS    0CL72               FIRST DOWNLOAD LINE                          
RPDLDEMS DS    CL1                 DEMO START DOUBLE QUOTE                      
RPDLDEM  DS    CL8                 DEMO                                         
RPDLDEME DS    CL1                 DEMO END   DOUBLE QUOTE                      
         DS    CL1                 FILLER                                       
RPDLMKTS DS    CL1                 MARKET START DOUBLE QUOTE                    
RPDLMKT  DS    CL4                 MARKET                                       
RPDLMKTE DS    CL1                 MARKET END   DOUBLE QUOTE                    
         DS    CL1                 FILLER                                       
RPDLMKNS DS    CL1                 MARKET NAME START DOUBLE QUOTE               
RPDLMKN  DS    CL24                MARKET NAME                                  
RPDLMKNE DS    CL1                 MARKET NAME END   DOUBLE QUOTE               
         DS    CL1                 FILLER                                       
RPDLDPTS DS    CL1                 DAYPART START DOUBLE QUOTE                   
RPDLDPT  DS    CL3                 DAYPART                                      
RPDLDPTE DS    CL1                 DAYPART END   DOUBLE QUOTE                   
         DS    CL1                 FILLER                                       
         ORG   REPD+132                                                         
RPDWNLN2 DS    0CL72               SECOND DOWNLOAD LINE                         
         DS    CL1                 FILLER                                       
RPDLCPP1 DS    CL10                COST PER POINT                               
         DS    CL1                 FILLER                                       
RPDLCPP2 DS    CL10                COST PER POINT                               
         DS    CL1                 FILLER                                       
RPDLCPP3 DS    CL10                COST PER POINT                               
         DS    CL1                 FILLER                                       
RPDLCPP4 DS    CL10                COST PER POINT                               
         DS    CL1                 FILLER                                       
RPDLEOL  DS    CL1                 END OF LINE                                  
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        COST PER POINT RECORD LAYOUT                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CPPD     DSECT                                                                  
CPPRECRD DS    0CL150              CPP RECORD                                   
CPPRMED  DS    CL1                 MEDIA                                        
         DS    CL1                 SEPARATOR                                    
CPPRCLT  DS    CL3                 CLIENT                                       
         DS    CL1                 SEPARATOR                                    
CPPRPRD  DS    CL3                 PRODUCT                                      
         DS    CL1                 FILLER                                       
CPPRDEM  DS    CL8                 DEMO                                         
         DS    CL1                 FILLER                                       
CPPRMKT  DS    CL4                 MARKET                                       
         DS    CL1                 FILLER                                       
CPPRMKN  DS    CL33                MARKET NAME                                  
         DS    CL1                 FILLER                                       
CPPRDMA  DS    CL4                 DMA ID                                       
         DS    CL1                 FILLER                                       
CPPRDPT  DS    CL3                 DAYPART                                      
         DS    CL1                 FILLER                                       
CPPRYR1  DS    CL4                 YEAR                                         
         DS    CL1                 FILLER                                       
CPPRQTR1 DS    CL1                 QUARTER                                      
         DS    CL1                 FILLER                                       
CPPRCPP1 DS    CL10                COST PER POINT                               
         DS    CL1                 FILLER                                       
CPPRYR2  DS    CL4                 YEAR                                         
         DS    CL1                 FILLER                                       
CPPRQTR2 DS    CL1                 QUARTER                                      
         DS    CL1                 FILLER                                       
CPPRCPP2 DS    CL10                COST PER POINT                               
         DS    CL1                 FILLER                                       
CPPRYR3  DS    CL4                 YEAR                                         
         DS    CL1                 FILLER                                       
CPPRQTR3 DS    CL1                 QUARTER                                      
         DS    CL1                 FILLER                                       
CPPRCPP3 DS    CL10                COST PER POINT                               
         DS    CL1                 FILLER                                       
CPPRYR4  DS    CL4                 YEAR                                         
         DS    CL1                 FILLER                                       
CPPRQTR4 DS    CL1                 QUARTER                                      
         DS    CL1                 FILLER                                       
CPPRCPP4 DS    CL10                COST PER POINT                               
         DS    CL11                FILLER                                       
         EJECT                                                                  
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DMREQHDR                                                                      
REQHDRD  DSECT                                                                  
       ++INCLUDE DMREQHDR                                                       
REQUEST  DS    0CL80               REQUEST CARD LAYOUT                          
REQJCLID DS    CL2                 JCL ID                                       
REQAGYID DS    CL2                 AGENCY ID                                    
         DS    CL1                 N/D                                          
REQSIN   DS    CL6                 SYSTEM INPUT NUMBER                          
         ORG   REQUEST+L'REQUEST                                                
*                                                                               
* PPEZFFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE PPEZFFFD                                                       
         PRINT ON                                                               
* DDGENTWA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* DDSPOOK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOK                                                        
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'125PPEZF99   10/15/97'                                      
         END                                                                    
