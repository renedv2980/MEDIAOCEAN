*          DATA SET ACREPPI02  AT LEVEL 005 AS OF 02/10/04                      
*******************************************************************             
* DO NOT DELETE THIS BOOK IT IS IN USE. SEE RAJIV GUPTA           *             
*******************************************************************             
*PHASE ACPI02A,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
         TITLE 'ACPI - POSPAY/A57 TABLE INFO REPORT'                            
ACPI02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACPI**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACPI02D,RC                                                       
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
         SPACE 5                                                                
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                      INITIALIZATIONS FOR THE RUN.           
         CLI   MODE,REQFRST              REQUEST FIRST                          
         BE    REQF                                                             
         CLI   MODE,REQLAST              REQUEST LAST                           
         BE    REQL                                                             
         CLI   MODE,RUNLAST              RUN LAST                               
         BE    RUNL                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS                                           
*        MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCGETOPT,C'N'                                                    
*                                                                               
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     R4,MCBXAREA                                                      
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'         SET WIDTH FOR REPORT                    
*                                                                               
         ZAP   PKRUNTOT,=P'0'           INITIALIZE RUN   TOTAL                  
*                                                                               
RUNFX    B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                     *           
**********************************************************************          
         SPACE 1                                                                
*                                                                               
REQF     DS    0H                                                               
         MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         ZAP   PKCPYTOT,=P'0'            INITIALIZE COUNT TOTAL                 
*                                                                               
         USING SPECD,R4                                                         
         L     R4,=A(ACCSPECS)                                                  
REQF10   CLI   0(R4),X'FF'         ARE WE AT THE END OF SPEC TABLE              
         BE    REQFX                                                            
*                                                                               
         CLC   ALPHAID,SPAGYCD                                                  
         BNE   REQF20                                                           
*                                                                               
         USING PLINED,R7                                                        
         LA    R7,XP                                                            
*                                                                               
         MVC   PALPHA,SPAGYCD      AGENCY ALPHA ID                              
         MVC   PSCACCT,SPQACCT     SC BANK ACCOUNT                              
         MVC   PBANKAC,SPBACCT     BANK ACCOUNT                                 
         MVC   PEYECAT,SPDTFNM     EYE CATCHER                                  
*                                                                               
         MVI   PPOSPAY,C'Y'                                                     
         CLI   SPPOSPY,X'00'       IS IT A POSPAY TRANSMISSION                  
         BNE   *+8                 X'80' X'81' YES                              
         MVI   PPOSPAY,C'N'                                                     
*                                                                               
         EDIT  SPBLKSZ,PBLOCK      BLOCK SIZE                                   
         EDIT  SPRECSZ,PRECSZ      RECORD SIZE                                  
*                                                                               
         ICM   R5,15,SPROUTE                                                    
         MVC   PROUTINE,0(R5)                                                   
         MVC   PDSNNM,SPDSNNM      DATA SET NAME                                
*                                                                               
         AP    PKCPYTOT,=P'1'      INCREMENT NO OF TABLE ENTRIES                
         AP    PKRUNTOT,=P'1'      INCREMENT NO OF FILE TOTAL ENTRIES           
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
REQF20   DS    0H                                                               
         LA    R4,SPECLNQ(R4)                                                   
         B     REQF10                                                           
*                                                                               
REQFX    B     EXIT                                                             
         EJECT                                                                  
         DROP  R7                                                               
**********************************************************************          
* REQUEST LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
REQL     DS    0H                                                               
         USING PLINED,R7                                                        
         LA    R7,XP                                                            
*                                                                               
         CP    PKCPYTOT,=P'0'                                                   
         BE    REQLX                                                            
         GOTO1 ACREPORT                  PRINT BLANK LINE                       
*                                                                               
         MVC   PRTTMSG,=CL10'TOTALS FOR'                                        
         MVC   PRTCPCD,ALPHAID                                                  
         MVI   PRTCOL,C':'                                                      
         MVC   PRTCMSG,=CL15'NUM OF RECDS = '                                   
         EDIT  (P4,PKCPYTOT),PRTCOUNT    RECD/COMPANY TOTAL                     
         GOTO1 ACREPORT                                                         
*                                                                               
         DROP  R7                                                               
REQLX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* RUN     LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
RUNL     DS    0H                                                               
         USING PLINED,R7                                                        
         LA    R7,XP                                                            
*                                                                               
         CP    PKRUNTOT,=P'0'                                                   
         BE    RUNLX                                                            
         GOTO1 ACREPORT                  PRINT BLANK LINE                       
*                                                                               
         MVC   XP(23),=C'TOTALS FOR THIS FILE = '                               
         EDIT  (P4,PKRUNTOT),(8,XP+24)   RECD/FILE    TOTAL                     
         GOTO1 ACREPORT                                                         
*                                                                               
         DROP  R7                                                               
RUNLX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
         SPACE 1                                                                
**********************************************************************          
* LOCAL STORAGE                                                      *          
**********************************************************************          
         SPACE 1                                                                
ADCONS   DS    0A                                                               
         EJECT                                                                  
RTNTAB   DC    CL40'******ROUTINE TABLE********'                                
ANBTC    DC    CL40'ANBTC - AMRCN NTNL BNK  81 BYTE'                            
ANBTCV   DC    CL40'ANBTC - AMRCN NTNL BNK VOIDS ONLY 60 BYTE'                  
BANKERS  DC    CL40'BANKERS - BANKERS TRUST DELAWARE 80 BYTE'                   
BANKOFNY DC    CL40'BANKOFNY - BANK OF NEW YORK 68 BYTE'                        
BARC     DC    CL40'BARC - BARCLAYS BANK OF CALIFORNIA 57 BYTE'                 
BOZJAC   DC    CL40'BOZELL AND JACOBS 80 BYTES'                                 
BNK1     DC    CL40'BNK1 - BANK ONE SECOND ROUTINE 80 BYTE'                     
BNY      DC    CL40'BNY - BANK OF NEW YORK 60 BYTE'                             
BNYDE    DC    CL40'BNYDE - BANK OF NEW YORK DELAWARE 65 BYTE'                  
BOA      DC    CL40'BOA - BANK OF AMERICA 80 BYTE'                              
BONE     DC    CL40'BONE - BANK ONE 80 BYTE'                                    
BROWN    DC    CL40'BROWN - BROWN BROTHERS 31 BYTE'                             
BTRUST   DC    CL40'BTRUST - BANKERS TRUST 24 BYTE'                             
CCMAN    DC    CL40'CCMAN - CORPORATE CASH MANAGEMENT 36 BYTE'                  
CHASE    DC    CL40'CHASE - CHASE BANK 33 BYTE'                                 
CHASE26  DC    CL40'CHASE26 - CHASE BANK 26 BYTE'                               
CHASE27  DC    CL40'CHASE27 - CHASE BANK 27 BYTE'                               
CHASE42  DC    CL40'CHASE42 - CHASE BANK 42 BYTE'                               
CHASE52  DC    CL40'CHASE52 - CHASE BANK 52 BYTE'                               
CHASE80  DC    CL40'CHASE80 - CHASE BANK 80 BYTE'                               
CHASE87  DC    CL40'CHASE87 - CHASE BANK 80 BYTE'                               
CHEM     DC    CL40'CHEM - CHEMICAL BANK 49 BYTE'                               
CHEM37   DC    CL40'CHEM37 - CHEMICAL BANK 37 BYTE'                             
CHEMDE   DC    CL40'CHEMDE - CHEMICAL BANK DELAWARE 27 BYTE'                    
CHML     DC    CL40'CHML - CHEMICAL BANK 52 BYTE'                               
CIBC     DC    CL40'CIBC - CANADA 80 BYTES'                                     
CITBK    DC    CL40'CITBK - CITIBANK 50 BYTE'                                   
CITIPR   DC    CL40'CITIPR - CITIBANK PUERTO RICO 50 BYTE'                      
CONBANK  DC    CL40'CONBANK - CONTINENTAL BANK 80 BYTE'                         
CTNB     DC    CL40'CTNB - CITY NATIONAL BANK 80 BYTE'                          
CTZN     DC    CL40'CTZN - CITIZEN''S BANK 80 BYTE'                             
EAB      DC    CL40'EAB - EUROPEAN AMERICAN BANK 70 BYTE'                       
FLEET    DC    CL40'FLEET - FLEET BANK 80 BYTE'                                 
FLEETB   DC    CL40'FLEETB - FLEET BANK 110 BYTE'                               
FLTC     DC    CL40'FLTC - FLEET BANK 80 BYTE'                                  
FNCHI    DC    CL40'FNCHI - FIRST NATIONAL OF CHICAGO 50 BYTE'                  
FNOMA    DC    CL40'FNOMA - FIRST NATIONS OF OMAHA 80 BYTE'                     
FORK     DC    CL40'FORK - FIRST BANK OF GRAND FORKS 80 BYTE'                   
FRSTPENN DC    CL40'FRSTPENN - FIRST PENNSYLVANIA BANK 28 BYTE'                 
FUNB     DC    CL40'FUNB - FIRST UNION NATIONAL BANK 80 BYTE'                   
FWACH    DC    CL40'FWACH - FIRST WACHOVIA BANK 29 BYTE'                        
FWBNK    DC    CL40'FWBNK - FIRST WACHOVIA BANK 80 BYTE'                        
GBNYMM   DC    CL40'GBNYMM - MARINE MIDLAND SHARE WITH TBC 80 BYTE'             
HARRIS   DC    CL40'HARRIS - HARRIS BANK CORP 46 BYTE'                          
HEN1     DC    CL40'HEN1 - HENDERSON 40 BYTE'                                   
HSBC     DC    CL40'HSBC - HSBC BANK 80 BYTE'                                   
JWT      DC    CL40'JWT - JWT RECORD 28 BYTE'                                   
LASALLE  DC    CL40'LA SALLE BANK   80  BYTE'                                   
MARMD    DC    CL40'MARMD - MARINE MIDLAND 80 BYTE'                             
MHANOV   DC    CL40'MHANOV - MANUFACTURERS HANOVER BANK 38 BYTE'                
MORGAN   DC    CL40'MORGAN - MORGAN GUARANTY 65 BYTE'                           
NATIONS  DC    CL40'NATIONS - NATIONS BANK 50 BYTE'                             
NBBC     DC    CL40'NBBC - NATIONAL BLVD BANK OF CHICAGO 29 BYTE'               
NBD      DC    CL40'NBD -  NATIONAL BANK DETROIT'                               
NTCRTN   DC    CL40'NTCRTN - NORTHERN TRUST BANK 40 BYTE'                       
NTHWST   DC    CL40'NTHWST - NORTHWESTERN NATIONAL 59 BYTE'                     
ROYAL    DC    CL40'ROYAL - ROYAL BANK 80 BYTE'                                 
RROY1    DC    CL40'RROY1 - ROSS ROY DETROIT AND CANADA 32 BYTE'                
SCO      DC    CL40'SCO - BANK OF NOVA SCOTIA 54 BYTE'                          
SEC      DC    CL40'SEC - SECURITY PACIFIC NATIONAL BANK 50 BYTE'               
SIGNET   DC    CL40'SIGNET -  SIGNET BANK 80 BYTE'                              
STATE    DC    CL40'STATE - STATE STREET BANK 96 BYTE'                          
SUTB     DC    CL40'SUTB - SUN TRUST BANK 80 BYTE    '                          
TEXAS    DC    CL40'TEXAS - TEXAS COMMERCE BANK 80 BYTE'                        
TLDA     DC    CL40'TLDA - TRACY-LOCKE IN-HOUSE 25 BYTE'                        
WACH     DC    CL40'WACH - WACHOVIA BANK 13 CHARA BANK ACCOUNT'                 
WISCRACI DC    CL40'WISCRACI - FIRST WISCONSIN RACINE 100 BYTE'                 
WISCWAUS DC    CL40'WISCWAUS - FIRST WISCONSIN WAUSAU 100 BYTE'                 
**********************************************************************          
* COMPANY TABLE - ACCOUNT SPECS                                      *          
* (10/96) DUE TO POSPAY CONFIGURATIONS THERE MAY BE TWO ENTRIES IN   *          
*         TABLE FOR THE SAME ACCPAK ACCOUNT.  THE DEFAULT(OR FIRST)  *          
*         TABLE ENTRY WILL BE THE POSPAY ENTRY.  THE SECOND ENTRY    *          
*         WILL ONLY BE USED IF QOPT7=N FOR NON POSPAY RUN            *          
**********************************************************************          
*                                                                               
ACCSPECS CSECT                                                                  
         DS    0CL(SPECLNQ)                                                     
       ++INCLUDE AC57TAB         57 SPEC TABLE                                  
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
        SPACE 1                                                                 
ACPI02D  DSECT                                                                  
VTYPES   DS    0A                                                               
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
PKCPYTOT DS    PL4                       RECORD/COMPANY TOTAL                   
PKRUNTOT DS    PL4                       RECORD/FILE    TOTAL                   
*                                                                               
CPYNM    DS    CL36                      COMPANY NAME                           
*                                                                               
*                                                                               
         EJECT                                                                  
**********************************************************************          
* PSPECS TABLE DSECT                                                 *          
**********************************************************************          
*                                                                               
SPECD    DSECT                                                                  
SPAGYCD  DS    CL2                 AGENCY CODE                                  
SPQACCT  DS    CL12                QACCOUNT                                     
SPBACCT  DS    CL20                BANK ACCOUNT                                 
SPDTFNM  DS    CL7                 DTF NAME                                     
SPPOSPY  DS    CL1                 POS PAY IDENTIFIER                           
SPPOSEQ  EQU   X'80'               ACCOUNT IS POS PAY                           
SPPOSAQ  EQU   X'01'               OVERRIDE FOR CHARGE,CHECK DODSTAB            
SPBLKSZ  DS    H                   BLOCK SIZE                                   
SPRECSZ  DS    H                   RECORD SIZE                                  
SPROUTE  DS    AL4                 ROUTINE NAME                                 
SPDSNNM  DS    CL20                DATA SET NAME                                
SPECLNQ  EQU   *-SPAGYCD           LENGTH                                       
         EJECT                                                                  
         SPACE 1                                                                
PLINED   DSECT                                                                  
PRTLIN   DS    0C                                                               
         DS    CL1                                                              
PALPHA   DS    CL2                 AGENCY CODE  (CMP)                           
         DS    CL3                                                              
PSCACCT  DS    CL12                SCCASH ACCOUNT                               
         DS    CL1                                                              
PBANKAC  DS    CL20                BANK ACCOUNT                                 
         DS    CL2                                                              
PEYECAT  DS    CL7                 DTF NAME                                     
         DS    CL9                                                              
PPOSPAY  DS    CL1                 POS PAY IDENTIFIER  Y/N                      
         DS    CL4                                                              
PBLOCK   DS    CL6                 BLOCK SIZE                                   
         DS    CL2                                                              
PRECSZ   DS    CL6                 RECORD SIZE                                  
         DS    CL4                                                              
PDSNNM   DS    CL20                DATA SET NAME                                
         DS    CL6                                                              
PROUTINE DS    CL40                ROUTINE NAME                                 
PLINLNQ  EQU   *-PRTLIN            LENGTH                                       
*                                                                               
         ORG   PLINED                                                           
PRTTMSG  DS    CL10'TOTALS FOR'          PRINT TOTALS MESSAGE                   
         DS    CL1                                                              
PRTCPCD  DS    CL2                       PRINT COMPANY CODE                     
         DS    CL2                                                              
PRTCOL   DS    CL1':'                                                           
         DS    CL5                                                              
PRTCMSG  DS    CL15'NUM OF RECDS = '                                            
PRTCOUNT DS    CL6                       NUM OF RECDS                           
         ORG                                                                    
*                                                                               
         EJECT                                                                  
***********************************************************************         
*               ++INCLUDES                                            *         
***********************************************************************         
*                                                                               
* ACBIGPRINTD                                                                   
* ACREPWORKD                                                                    
* ACGENFILE                                                                     
* ACGENMODES                                                                    
* DDLOGOD                                                                       
* ACMASTD                                                                       
* DDMASTD                                                                       
* DDBIGBOX                                                                      
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPPI02 02/10/04'                                      
         END                                                                    
