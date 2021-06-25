*          DATA SET DESYSCODE  AT LEVEL 002 AS OF 07/20/16                      
*PROCESS USING(WARN(15))                                                        
*PHASE DESYSCA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINT132              <=== NEED THIS TO WRITE TO SYSPRIN2              
*INCLUDE SORTER                                                                 
***********************************************************************         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'FUSION SYSCODE FILE READER'                                     
DESYSC   CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
                                                                                
         PRINT NOGEN                                                            
         NBASE 0,*DESYSC*,=V(REGSAVE),R9                                        
                                                                                
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)       SYSPRINT (TO LOG DATASET)                    
                                                                                
         OPEN  (OUT1,(OUTPUT))    OPEN OUTPUT TAPE                              
                                                                                
         L     RF,=V(DATAMGR)                                                   
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'DEMO',                   +        
               =C'NCTFILE NDEMDIRRX'                                            
                                                                                
         BRAS  RE,READCARD                                                      
         BRAS  RE,READFILE                                                      
         XIT1                                                                   
         EJECT                                                                  
         SPACE 3                                                                
                                                                                
***********************************************************************         
READCARD NTR1                                                                   
READCD10 GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         MVC   P(L'CARD),CARD                                                   
         L     RF,VPRINTER         PRINT THE CONTROL CARD                       
         BASR  RE,RF                                                            
         CLI   CARD,C'*'           COMMENT CARD?                                
         BE    READCD10            YES: IGNORE                                  
         CLC   =C'/*',CARD                                                      
         BE    MISCARD             EOF                                          
*                                                                               
         CLC   =C'MONTHS=',CARD    MONTHS= CARD?                                
         BNE   INVCARD             NO                                           
         MVC   MONSFILT,=H'36'                                                  
         CLC   =C'ALL',CARD+7      MONTHS=ALL = 36 MONTHS                       
         BE    DONECARD            YES: NO SYSCODE FILTER                       
         GOTO1 =V(NUMVAL),DMCB,CARD+7,(2,0)  VALIDATE THE MONTH #               
         CLI   DMCB,0              VALID NUMERIC?                               
         BNE   INVCARD             YES                                          
         MVC   MONSFILT,DMCB+6     SAVE THE MONTHS FILTER                       
         B     DONECARD                                                         
*                                                                               
INVCARD  MVC   P(34),=C'*** ERROR *** INVALID CONTROL CARD'                     
         L     RF,VPRINTER                                                      
         BASR  RE,RF                                                            
         B     BAD                                                              
*                                                                               
MISCARD  MVC   P(33),=C'*** ERROR *** MISSING MONTHS CARD'                      
         L     RF,VPRINTER                                                      
         BASR  RE,RF                                                            
         B     BAD                                                              
*                                                                               
DONECARD DS    0H                                                               
         MVC   P(26),=C'CARD VALIDATION SUCCESSFUL'                             
         L     RF,VPRINTER                                                      
         BASR  RE,RF                                                            
         XIT1                                                                   
***********************************************************************         
READFILE NTR1                                                                   
         LA    R3,KEY                                                           
         USING DFUEKEY,R3                                                       
         XC    SAVEBOOK,SAVEBOOK                                                
         XC    SAVEMKT,SAVEMKT                                                  
         XC    NUMBOOKS,NUMBOOKS                                                
         XC    KEY,KEY                                                          
*                                                                               
         MVI   DFUECODE,DFUECODQ   RECORD TYPE 'G'                              
         MVI   DFUEMED,C'T'        MEDIA: TV                                    
         MVI   DFUESRC,C'F'        SOURCE: FUSION                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'DEMDIR',KEY,KEY               
         CLI   DMCB+8,0            RECORD FOUND?                                
         BE    READ20              YES                                          
         DC    H'0'                FATAL DATAMGR ERROR                          
*                                                                               
READ10   GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRSEQ'),=C'DEMDIR',KEY,KEY               
         CLI   DMCB+8,0            RECORD FOUND?                                
         BE    READ20              YES                                          
         DC    H'0'                FATAL DATAMGR ERROR                          
*                                                                               
READ20   CLC   =C'GTF',KEY                                                      
         BNE   READX               NO: IT WILL BE ADDED                         
                                                                                
         AP    UERECS#,=P'1'       INCREMENT RECORD COUNT                       
         BRAS  RE,PARSEREC                                                      
***      CLC   NUMBOOKS,=H'36'     ONLY READ 36 BOOKS / 3 YEARS                 
         CLC   NUMBOOKS,MONSFILT   READ UP TO MONTHS SPECIFIED IN CARD          
         BH    READX                                                            
*                                                                               
         B     READ10                                                           
READX    XIT1                                                                   
*                                                                               
***********************************************************************         
PARSEREC NTR1                                                                   
         LA    R3,KEY                                                           
         USING DFUEKEY,R3                                                       
         CLC   SAVEBOOK,DFUEBOOK                                                
         BE    PARSER10                                                         
         ZICM  R0,NUMBOOKS,(3)                                                  
         AHI   R0,1                                                             
         STCM  R0,3,NUMBOOKS                                                    
         CLC   NUMBOOKS,MONSFILT   READ UP TO MONTHS SPECIFIED IN CARD          
         BH    PARSERCX                                                         
* DELIMITED FILE BY @ EVERY BOOK CHANGE SO THAT THE JAVA CAMEL PROCESS          
* CAN SPLIT UP THE FILE PROCESSING TO DECREASE MEMORY FOOTPRINT                 
*                                                                               
         XC    P,P                                                              
         MVI   P,C'@'                                                           
         PUT   OUT1,P                                                           
         XC    P,P                                                              
*                                                                               
PARSER10 CLC   NUMBOOKS,MONSFILT                                                
         BH    PARSERCX                                                         
PARSER20 MVC   SAVEBOOK,DFUEBOOK                                                
         MVC   SAVEMKT,DFUEMKT                                                  
         MVC   P(43),=43C','                                                    
         XC    DFUEBOOK,=X'FFFF'                                                
         GOTO1 =V(DATCON),DMCB,(3,DFUEBOOK),(6,P_BOOK)                          
         EDIT  (B2,DFUEMKT),(3,P_UE_DMA#)                                       
         EDIT  (B2,DFUESYSC),(4,P_UE_SYSCODE),ALIGN=RIGHT,FILL=0                
         MVC   P_UE_NETWORK_ID,DFUESTN                                          
         OC    P_UE_NETWORK_ID,=4C' '                                           
         CLC   P_UE_NETWORK_ID,=4C' '       ONLY PASS SPACE                     
         BE    *+14                         OR NUMERIC NETWORKS                 
         CLC   P_UE_NETWORK_ID,=C'0000'     ELSE, MIGHT BE BAD                  
         BL    PARSERCX                                                         
         EDIT  (B3,DFUEAIUE),(10,P_UE_AI_UE),ALIGN=RIGHT,FILL=0                 
         EDIT  (B3,DFUECAUE),(10,P_UE_CARRIAGE_UE),ALIGN=RIGHT,FILL=0           
**       L     RF,VPRINTER                                                      
**       BASR  RE,RF                                                            
         PUT   OUT1,P                                                           
PARSERCX XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         CLOSE (OUT1,)                                                          
         EJECT                                                                  
*                                                                               
BAD      DS    0H                                                               
         MVC   P(20),=C'*** ERRORS FOUND ***'                                   
         L     RF,VPRINTER                                                      
         BASR  RE,RF               PRINT TO SYSPRINT LOG DATASET                
         LHI   R3,8                CONVERSION ERROR(S) OCCURRED: SET RC         
*                                                                               
*                                                                               
         XBASE RC=(R3)             RC = 0 IS OKAY, RC = 8 IS FATAL              
         EJECT                                                                  
*                                                                               
VPRINTER DC    V(PRINTER)                                                       
         SPACE 3                                                                
OUT1     DCB   DDNAME=OTAPE,DSORG=PS,RECFM=FB,MACRF=(PM),              X        
               LRECL=42,BLKSIZE=8400                                            
         SPACE 3                                                                
NUMBOOKS DS    XL2                                                              
*                                                                               
SVGRPKEY DS    0XL4                                                             
SAVEBOOK DS    XL2                                                              
SAVEMKT  DS    XL2                                                              
*                                                                               
DMCB     DS    6F                                                               
DUB      DS    D                                                                
WORK     DS    CL17                FOR EDIT MACRO                               
CARD     DS    CL80                SYSIN PARAMETER CARD                         
UERECS#  DC    PL8'0'              NUMBER OF UE RECORDS GENERATED               
UERECS#R DC    PL8'0'              NUMBER OF DUPLICATE UE RECS SKIPPED          
MSORECS# DC    PL8'0'              NUMBER OF MSO RECORDS GENERATED              
ERRFLAG  DC    C'N'                ASSUME NO ERRORS WILL OCCUR                  
REPLACEG DC    C'N'                ASSUME NO REPLACEMENT OF "G" KEYS            
MONSFILT DC    H'0'                MONTHS FILTER (IF NON-ZERO)                  
THREE    DS    XL3                                                              
KEY      DS    XL23                DEMDIR KEY                                   
ELEMENT  DS    XL255                                                            
         EJECT                                                                  
         SPACE 3                                                                
UTL      DS    0D                                                               
         DC    4X'00',X'0C'        UTL FOR DEMO SYSTEM                          
         SPACE 2                                                                
         DS    0L                                                               
         DC    CL16'****DETFUESSB***'                                           
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
         SPACE 3                                                                
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
       ++INCLUDE DETFUERECD                                                     
         EJECT                                                                  
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         ORG   P                                                                
PREC     DS        0C                                                           
P_BOOK             DS CL6          BOOK                                         
         DS C                                                                   
P_UE_DMA#          DS CL3          NIELSEN DMA NUMBER (+400)                    
         DS C                                                                   
P_UE_SYSCODE       DS CL4          SYSCODE                                      
         DS C                                                                   
P_UE_NETWORK_ID    DS CL4          NETWORK ID                                   
         DS C                                                                   
P_UE_AI_UE         DS CL10         AD-INSERTABLE UNIVERSE ESTIMATE              
         DS C                                                                   
P_UE_CARRIAGE_UE   DS CL10         CARRIAGE UNIVERSE ESTIMATE                   
*                                                                               
PRECX    EQU       *                                                            
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DESYSCODE 07/20/16'                                      
         END                                                                    
